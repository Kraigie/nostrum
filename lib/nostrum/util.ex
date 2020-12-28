defmodule Nostrum.Util do
  @moduledoc """
  Utility functions
  """

  alias Nostrum.{Api, Constants, Snowflake}
  alias Nostrum.Shard.Session
  alias Nostrum.Struct.WSState

  require Logger

  @doc """
  Helper for defining all the methods used for struct and encoding transformations.

  ## Example
  ``` Elixir
  Nostrum.Util.nostrum_struct(%{
    author: User,
    mentions: [User],
    mention_roles: [User],
    embeds: [Embed]
  })
  ```
  """
  defmacro nostrum_struct(body) do
    quote do
      @derive [Poison.Encoder]
      defstruct Map.keys(unquote(body))

      def p_encode do
        encoded =
          for {k, v} <- unquote(body), v != nil, into: %{} do
            case v do
              [v] -> {k, [v.p_encode]}
              v -> {k, v.p_encode}
            end
          end

        struct(__ENV__.module, encoded)
      end

      def to_struct(map) do
        alias Nostrum.Util

        new_map =
          for {k, v} <- unquote(body), into: %{} do
            case v do
              nil -> {k, Map.get(map, k)}
              [v] -> {k, Util.enum_to_struct(Map.get(map, k), v)}
              v -> {k, apply(v, :to_struct, [Map.get(map, k)])}
            end
          end

        struct(__ENV__.module, new_map)
      end
    end
  end

  @doc """
  Returns the number of milliseconds since unix epoch.
  """
  @spec now() :: integer
  def now do
    DateTime.utc_now()
    |> DateTime.to_unix(:millisecond)
  end

  @doc """
  Returns the number of microseconds since unix epoch.
  """
  @spec usec_now() :: integer
  def usec_now do
    DateTime.utc_now()
    |> DateTime.to_unix(:microsecond)
  end

  @doc """
  Returns the current date as an ISO formatted string.
  """
  @spec now_iso() :: String.t()
  def now_iso do
    DateTime.utc_now()
    |> DateTime.to_iso8601()
  end

  @doc false
  def list_to_struct_list(list, struct) when is_list(list) do
    Enum.map(list, &struct.to_struct(&1))
  end

  def enum_to_struct(nil, _struct), do: nil
  def enum_to_struct(enum, struct) when is_list(enum), do: Enum.map(enum, &struct.to_struct(&1))

  def enum_to_struct(enum, struct) when is_map(enum) do
    for {k, v} <- enum, into: %{} do
      {k, struct.to_struct(v)}
    end
  end

  @doc """
  Returns the number of shards.

  This is not the number of currently active shards, but the number of shards specified
  in your config.
  """
  @spec num_shards() :: integer
  def num_shards do
    num =
      with :auto <- Application.get_env(:nostrum, :num_shards),
           {_url, shards} <- gateway(),
           do: shards

    if num == nil, do: 1, else: num
  end

  @doc false
  def bangify_find(to_bang, find, cache_name) do
    case to_bang do
      {:ok, res} ->
        res

      {:error} ->
        raise(Nostrum.Error.CacheError, finding: find, cache_name: cache_name)

      {:error, _other} ->
        raise(Nostrum.Error.CacheError, finding: find, cache_name: cache_name)
    end
  end

  @doc """
  Returns the gateway url and shard count for current websocket connections.

  If by chance no gateway connection has been made, will fetch the url to use and store it
  for future use.
  """
  @spec gateway() :: {String.t(), integer}
  def gateway do
    case :ets.lookup(:gateway_url, "url") do
      [] -> get_new_gateway_url()
      [{"url", url, shards}] -> {url, shards}
    end
  end

  defp get_new_gateway_url do
    case Api.request(:get, Constants.gateway_bot(), "") do
      {:error, %{status_code: 401}} ->
        raise("Authentication rejected, invalid token")

      {:error, %{status_code: code, message: message}} ->
        raise(Nostrum.Error.ApiError, status_code: code, message: message)

      {:ok, body} ->
        body = Poison.decode!(body)

        "wss://" <> url = body["url"]
        shards = if body["shards"], do: body["shards"], else: 1

        :ets.insert(:gateway_url, {"url", url, shards})
        {url, shards}
    end
  end

  @doc """
  Converts a map into an atom-keyed map.

  Given a map with variable type keys, returns the same map with all keys as `atoms`.

  This function will attempt to convert keys to an existing atom, and if that fails will default to
  creating a new atom while displaying a warning. The idea here is that we should be able to see
  if any results from Discord are giving variable keys. Since we *will* define all
  types of objects returned by Discord, the amount of new atoms created *SHOULD* be 0. 👀
  """
  @spec safe_atom_map(map) :: map
  def safe_atom_map(term) do
    cond do
      is_map(term) ->
        for {key, value} <- term, into: %{}, do: {maybe_to_atom(key), safe_atom_map(value)}

      is_list(term) ->
        Enum.map(term, fn item -> safe_atom_map(item) end)

      true ->
        term
    end
  end

  @doc """
  Attempts to convert a string to an atom.

  If atom does not currently exist, will warn that we're doing an unsafe conversion.
  """
  @spec maybe_to_atom(atom | String.t()) :: atom
  def maybe_to_atom(token) when is_atom(token), do: token

  def maybe_to_atom(token) do
    String.to_existing_atom(token)
  rescue
    _ ->
      Logger.debug(fn -> "Converting string to non-existing atom: #{token}" end)
      String.to_atom(token)
  end

  # Generic casting function
  @doc false
  @spec cast(term, module | {:list, term} | {:struct, term} | {:index, [term], term}) :: term
  def cast(value, type)
  def cast(nil, _type), do: nil

  def cast(values, {:list, type}) when is_list(values) do
    Enum.map(values, fn value ->
      cast(value, type)
    end)
  end

  # Handles the case where the given term is already indexed
  def cast(values, {:index, _index_by, _type}) when is_map(values), do: values

  def cast(values, {:index, index_by, type}) when is_list(values) do
    values
    |> Enum.into(%{}, &{&1 |> get_in(index_by) |> cast(Snowflake), cast(&1, type)})
  end

  def cast(value, {:struct, module}) when is_map(value) do
    module.to_struct(value)
  end

  def cast(value, module) do
    case module.cast(value) do
      {:ok, result} -> result
      _ -> value
    end
  end

  @doc false
  @spec fullsweep_after() :: {:fullsweep_after, non_neg_integer}
  def fullsweep_after do
    {:fullsweep_after,
     Application.get_env(
       :nostrum,
       :fullsweep_after_default,
       :erlang.system_info(:fullsweep_after) |> elem(1)
     )}
  end

  @doc """
  Gets the latency of the shard connection from a `Nostrum.Struct.WSState.t()` struct.

  Returns the latency in milliseconds as an integer, returning nil if unknown.
  """
  @spec get_shard_latency(WSState.t()) :: non_neg_integer | nil
  def get_shard_latency(%WSState{last_heartbeat_ack: nil}), do: nil

  def get_shard_latency(%WSState{last_heartbeat_send: nil}), do: nil

  def get_shard_latency(%WSState{} = state) do
    latency = DateTime.diff(state.last_heartbeat_ack, state.last_heartbeat_send, :millisecond)
    max(0, latency + if(latency < 0, do: state.heartbeat_interval, else: 0))
  end

  @doc """
  Gets the latencies of all shard connections.

  Calls `get_shard_latency/1` on all shards and returns a map whose keys are
  shard nums and whose values are latencies in milliseconds.
  """
  @spec get_all_shard_latencies :: %{WSState.shard_num() => non_neg_integer | nil}
  def get_all_shard_latencies do
    ShardSupervisor
    |> Supervisor.which_children()
    |> Enum.filter(fn {_id, _pid, _type, [modules]} -> modules == Nostrum.Shard end)
    |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.which_children(pid) end)
    |> List.flatten()
    |> Enum.map(fn {_id, pid, _type, _modules} -> Session.get_ws_state(pid) end)
    |> Enum.reduce(%{}, fn s, m -> Map.put(m, s.shard_num, get_shard_latency(s)) end)
  end

  @doc """
  Since we're being sacrilegious and converting strings to atoms from the WS, there will be some
  atoms that we see that aren't defined in any Discord structs. This method mainly serves as a
  means to define those atoms once so the user isn't warned about them in the
  `Nostrum.Util.maybe_to_atom/1` function when they are in fact harmless.
  """
  def unused_atoms do
    [
      :active,
      :audio,
      :audio_codec,
      :audio_ssrc,
      :channel_overrides,
      :convert_emoticons,
      :detect_platform_accounts,
      :developer_mode,
      :enable_tts_command,
      :encodings,
      :experiments,
      :friend_source_flags,
      :friend_sync,
      :guild_positions,
      :inline_attachment_media,
      :inline_embed_media,
      :last_message_id,
      :locale,
      :max_bitrate,
      :media_session_id,
      :message_display_compact,
      :message_notifications,
      :mobile_push,
      :modes,
      :muted,
      :recipients,
      :referenced_message,
      :render_embeds,
      :render_reactions,
      :require_colons,
      :restricted_guilds,
      :rid,
      :rtx_ssrc,
      :scale_resolution_down_by,
      :show_current_game,
      :suppress_everyone,
      :theme,
      :video,
      :video_codec,
      :video_ssrc,
      :visibility
    ]
  end
end
