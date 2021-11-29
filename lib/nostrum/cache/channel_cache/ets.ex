defmodule Nostrum.Cache.ChannelCache.ETS do
  @table_name :nostrum_channels
  @moduledoc """
  An ETS-based cache for channels.

  The supervisor defined by this module will set up the ETS table associated
  with it.

  The default table name under which channels are cached is `#{@table_name}`.
  In addition to the cache behaviour implementations provided by this module,
  you can also call regular ETS table methods on it, such as `:ets.info`.

  Note that users should not call the functions not related to this specific
  implementation of the cache directly. Instead, call the functions of
  `Nostrum.Cache.ChannelCache` directly, which will dispatch to the configured
  cache.
  """
  @moduledoc since: "0.5"

  @behaviour Nostrum.Cache.ChannelCache

  alias Nostrum.Cache.ChannelCache
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Channel
  alias Nostrum.Util
  import Nostrum.Snowflake, only: [is_snowflake: 1]
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the cache's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(tabname(), [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the ETS table name used for the cache."
  @spec tabname :: atom()
  def tabname, do: @table_name

  @doc ~S"""
  Retrieves a channel from the cache.

  Internally, the ChannelCache process only stores
  `t:Nostrum.Struct.Channel.dm_channel/0` references. To get channel
  information, a call is made to a `Nostrum.Cache.GuildCache`.

  If successful, returns `{:ok, channel}`. Otherwise, returns `{:error, reason}`

  ## Example
  ```elixir
  case Nostrum.Cache.ChannelCache.get(133333333337) do
    {:ok, channel} ->
      "We found " <> channel.name
    {:error, _reason} ->
      "Donde esta"
  end
  ```
  """
  @impl ChannelCache
  @spec get(Channel.id() | Nostrum.Struct.Message.t()) ::
          {:ok, Channel.t()} | {:error, ChannelCache.reason()}
  def get(%Nostrum.Struct.Message{channel_id: channel_id}), do: get(channel_id)

  def get(id) when is_snowflake(id) do
    case lookup(id) do
      {:ok, channel} -> {:ok, convert(channel)}
      error -> error
    end
  end

  @doc ~S"""
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @impl ChannelCache
  @spec get!(Channel.id() | Nostrum.Struct.Message.t()) :: no_return | Channel.t()
  def get!(%Nostrum.Struct.Message{channel_id: channel_id}), do: get!(channel_id)
  def get!(id) when is_snowflake(id), do: id |> get |> Util.bangify_find(id, __MODULE__)

  @doc false
  @impl ChannelCache
  @spec create(map) :: Channel.t()
  def create(channel) do
    :ets.insert(tabname(), {channel.id, channel})
    convert(channel)
  end

  @doc false
  @impl ChannelCache
  @spec update(Channel.t()) :: :noop | {Channel.t(), Channel.t()}
  def update(channel) do
    case lookup(channel.id) do
      {:ok, old_channel} ->
        {convert(old_channel), convert(channel)}

      _ ->
        :noop
    end
  end

  @doc false
  @impl ChannelCache
  @spec delete(Channel.id()) :: :noop | Channel.t()
  def delete(id) do
    case lookup(id) do
      {:ok, channel} ->
        :ets.delete(tabname(), id)
        convert(channel)

      _ ->
        :noop
    end
  end

  @doc false
  @impl ChannelCache
  @spec lookup(Channel.id()) :: {:error, :channel_not_found} | {:ok, map}
  def lookup(id) do
    case :ets.lookup(tabname(), id) do
      [] ->
        [channel_id: id]
        |> GuildCache.select_by(fn %{channels: channels} ->
          Map.get(channels, id, {:error, :id_not_found})
        end)
        |> case do
          {:error, :id_not_found} ->
            {:error, :channel_not_found}

          res ->
            res
        end

      [{^id, channel}] ->
        {:ok, channel}
    end
  end

  @doc false
  defp convert(%{__struct__: _} = struct), do: struct
  defp convert(map), do: Channel.to_struct(map)
end
