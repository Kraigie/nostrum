defmodule Nostrum.Shard.Supervisor do
  @moduledoc """
  Supervises shard processes.

  ## Implementation

  As events are sent to the shard, the following happens:

  1. Shard looks to see what type of event it is, only dispatch events are sent
     to the producer.

  2. If the event is a `Dispatch`, the payload is converted to an atom-keyed map.
     This is done because over ETF (which Nostrum uses), map keys are sometimes
     binaries and sometimes strings, making it a real headache. Additionally,
     with atom keys, we can use the `Map.key` notation. This is normally
     considered unsafe but a debug messages will be emitted if a key is unsafely
     converted to an atom. In this way we can ensure that our atom table is not
     growing unbounded.

  3. The payload is then written to the cache. To make sure we're not overrunning
     the cache, especially at startup with `request_guild_members` or other heavy
     payloads, this is done in the shard itself.

  4. The cache updates itself from the new data. In some cases, such as update or
     delete events, it may send out a second "old" object as well, that helps the
     library user to determine what changed.

  5. After writing to the cache, the shard `send`s out the event after going
     through the cache to all subscribed processes. In general, the payload will
     often match the payload described by the official Discord API documentation.

  6. The shard instructs the websocket client that it's ready to read more data.
     This prevents flooding the shard with messages that it may not be able to
     handle yet, thus growing the message queue and the memory usage.
  """

  @typedoc """
  Shard number(`shard_id`). Range is `0..total_shards-1`.
  """
  @type shard_num :: non_neg_integer()
  @typedoc """
  Total shard count(`num_shards`).
  """
  @type total_shards :: pos_integer()
  @typedoc """
  Represents gateway resume information.
  """
  @type resume_information :: %{
          shard_num: shard_num(),
          total_shards: total_shards(),
          gateway: String.t(),
          bot_options: Nostrum.Bot.bot_options(),
          resume_gateway: String.t() | nil,
          session: String.t(),
          seq: pos_integer()
        }

  use Supervisor

  alias Nostrum.Bot
  alias Nostrum.Error.CacheError
  alias Nostrum.Shard
  alias Nostrum.Shard.Connector
  alias Nostrum.Shard.Session
  alias Nostrum.Store.GuildShardMapping
  alias Nostrum.Util

  require Logger

  defp cast_shard_range(gateway_shards, :auto), do: {1, gateway_shards, gateway_shards}
  defp cast_shard_range(gateway_shards, gateway_shards), do: {1, gateway_shards, gateway_shards}

  defp cast_shard_range(gateway_shards, count) when is_integer(count) and count > 0 do
    Logger.warning(
      "Configured shard count (#{count}) " <>
        "differs from Discord Gateway's recommended shard count (#{gateway_shards}). " <>
        "Consider using `num_shards: :auto` option in your bot options."
    )

    {1, count, count}
  end

  defp cast_shard_range(_gateway_shards, {lowest, highest, total} = range)
       when is_integer(lowest) and is_integer(highest) and is_integer(total) and
              lowest <= highest and highest <= total do
    range
  end

  def start_link(%{name: bot_name} = bot_options) do
    prev_value = Bot.set_bot_name(bot_name)
    {_url, gateway_shard_count} = Util.gateway()
    _ = Bot.set_bot_name(prev_value)

    shard_children =
      case Map.get(bot_options, :shards, :auto) do
        :manual ->
          []

        value ->
          {lowest, highest, total} = cast_shard_range(gateway_shard_count, value)

          Enum.map(lowest..highest, &shard_child_spec(&1 - 1, total, bot_options))
      end

    Supervisor.start_link(__MODULE__, shard_children)
  end

  def update_status(status, activity) do
    fetch_shard_sup_pid()
    |> Util.get_children_pids(Shard)
    |> Enum.map(&Util.get_child_pid(&1, Session))
    |> Enum.map(&Session.update_status(&1, status, activity))
  end

  def update_voice_state(guild_id, channel_id, self_mute, self_deaf) do
    case GuildShardMapping.get(guild_id) do
      nil ->
        raise CacheError, key: guild_id, cache_name: GuildShardMapping

      shard_num ->
        shard_num
        |> fetch_shard_session_pid()
        |> Session.update_voice_state(guild_id, channel_id, self_mute, self_deaf)
    end
  end

  @doc false
  def init(shard_children) do
    children = [Connector | shard_children]
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 3, max_seconds: 60)
  end

  @doc false
  def fetch_shard_sup_pid do
    Util.get_child_pid(Bot.fetch_bot_pid(), __MODULE__)
  end

  @doc false
  def fetch_shard_pid(shard_num) do
    Util.get_child_pid(fetch_shard_sup_pid(), shard_num)
  end

  @doc false
  def fetch_shard_session_pid(shard_num) do
    Util.get_child_pid(fetch_shard_pid(shard_num), Session)
  end

  @doc false
  def shard_child_spec(shard_num, total_shards, bot_options) do
    {gateway, _gateway_shard_count} = Util.gateway()

    shard_child_spec(%{
      gateway: gateway,
      shard_num: shard_num,
      total_shards: total_shards,
      bot_options: bot_options
    })
  end

  @doc false
  def shard_child_spec(%{shard_num: shard_num} = shard_opts) do
    Supervisor.child_spec({Shard, shard_opts}, id: shard_num)
  end

  @doc """
  Disconnects the shard with the given shard number from the Gateway.

  This function returns `t:resume_information/0` which can be provided
  to `reconnect/1` to reconnect a shard to the gateway and (attempt) to
  catch up on any missed events.

  ### Examples

  ```elixir
  iex> Nostrum.Shard.Supervisor.disconnect(4)
  %{shard_num: 4, ...}
  ```
  """
  @doc since: "0.10.0"
  @spec disconnect(Supervisor.supervisor(), :gen_statem.server_ref() | nil, shard_num()) ::
          resume_information()
  def disconnect(supervisor \\ fetch_shard_sup_pid(), shard_session \\ nil, shard_num) do
    shard_session = shard_session || fetch_shard_session_pid(shard_num)
    resume_info = Session.disconnect(shard_session)
    :ok = Supervisor.delete_child(supervisor, resume_info.shard_num)
    resume_info
  end

  @doc """
  Spawns a shard with the specified number and connects it to the discord gateway.
  """
  @spec connect(Supervisor.supervisor(), shard_num(), total_shards(), Bot.bot_options()) ::
          Supervisor.on_start_child()
  def connect(supervisor \\ fetch_shard_sup_pid(), shard_num, total_shards, bot_options) do
    Supervisor.start_child(
      supervisor,
      shard_child_spec(shard_num, total_shards, bot_options)
    )
  end

  @doc """
  Reconnect to the gateway using the provided `t:resume_information/0`.

  Resuming is performed by the gateway on a best effort basis, it is not
  guaranteed that a resume will work (though Nostrum will handle failed attempts
  at a resumption). If a reconnect is successful, any events received during the
  reconnection period should be received. If the reconnect fails, events
  received between the disconnect and re-authentication may be lost.

  For more information about resuming sessions, visit
  [the Discord Developer Portal](https://discord.com/developers/docs/topics/gateway#resuming).

  ### Examples

  ```elixir
  iex> resume = Nostrum.Shard.Supervisor.disconnect(4)
  %{shard_num: 4, ...}
  iex> Nostrum.Shard.Supervisor.reconnect(resume)
  ```
  """
  @doc since: "0.10.0"
  @spec reconnect(Supervisor.supervisor(), resume_information()) :: Supervisor.on_start_child()
  def reconnect(
        supervisor \\ fetch_shard_sup_pid(),
        %{
          shard_num: _shard_num,
          total_shards: _total_shards,
          gateway: _gateway,
          bot_options: _bot_options,
          resume_gateway: _resume_gateway,
          seq: _seq,
          session: _session
        } = opts
      ) do
    Supervisor.start_child(
      supervisor,
      shard_child_spec(opts)
    )
  end
end
