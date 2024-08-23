defmodule Nostrum.Util do
  @moduledoc """
  Utility functions
  """

  @gateway_url_key :nostrum_gateway_url

  alias Nostrum.{Api, Constants, Snowflake}
  alias Nostrum.Shard.Session
  alias Nostrum.Struct.WSState

  require Logger

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
  Returns the total amount of shards as per the configuration.

  ## Return value

  - If you specified your shards as `:auto`, the return value will be the
  recommended number of shards as given by the gateway.

  - If you explicitly specified your shard numbers as an integer, it will be
  the given number.

  - If you specified your shards in the form `{lowest, highest, total}` to
  start a specific range of the total shards you want to start, this will be
  the `total` value.

  Should Discord not supply us with any shard information, this will return
  `1`.

  Note that this is not the number of currently active shards, but the number
  of shards specified in your config.
  """
  @spec num_shards() :: pos_integer()
  def num_shards do
    num =
      with :auto <- Application.get_env(:nostrum, :num_shards, :auto),
           {_url, shards} <- gateway() do
        shards
      end

    case num do
      {_lowest, _highest, total} -> total
      :manual -> 0
      nil -> 1
    end
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
    case :persistent_term.get(@gateway_url_key, nil) do
      nil -> get_new_gateway_url()
      result -> result
    end
  end

  defp get_new_gateway_url do
    case Api.request(:get, Constants.gateway_bot()) do
      {:error, %{status_code: 401}} ->
        raise("Authentication rejected, invalid token")

      {:error, %{status_code: code, response: %{message: message}}} ->
        raise(Nostrum.Error.ApiError, status_code: code, message: message)

      {:ok, body} ->
        body = Jason.decode!(body)

        "wss://" <> url = body["url"]
        shards = if body["shards"], do: body["shards"], else: 1

        :persistent_term.put(@gateway_url_key, {url, shards})
        {url, shards}
    end
  end

  @doc """
  Converts a map into an atom-keyed map.

  Given a map with variable type keys, returns the same map with all keys as `atoms`.
  To support maps keyed with integers (such as in Discord's interaction data),
  binaries that appear to be integers will be parsed as such.

  This function will attempt to convert keys to an existing atom, and if that fails will default to
  creating a new atom while displaying a warning. The idea here is that we should be able to see
  if any results from Discord are giving variable keys. Since we *will* define all
  types of objects returned by Discord, the amount of new atoms created *SHOULD* be 0. 👀
  """
  @spec safe_atom_map(map) :: map
  def safe_atom_map(term) do
    case term do
      # to handle the rare occasion that discord leaks a `:__struct__` key
      # rather than outright crashing, we'll just log a warning and continue
      %{__struct__: struct_name} ->
        Logger.warning(
          "Discord's gateway leaked a struct with name #{inspect(struct_name)}, please report this to the library maintainer"
        )

        term = Map.from_struct(term)
        for {key, value} <- term, into: %{}, do: {maybe_to_atom(key), safe_atom_map(value)}

      # if we have a regular map
      %{} ->
        for {key, value} <- term, into: %{}, do: {maybe_to_atom(key), safe_atom_map(value)}

      # if we have a non-empty list
      [_ | _] ->
        Enum.map(term, fn item -> safe_atom_map(item) end)

      _ ->
        term
    end
  end

  @doc """
  Attempts to convert a string to an atom.

  Binary `token`s that consist of digits are assumed to be snowflakes, and will
  be parsed as such.

  Some maps sent from Discord are integer-indexed, for these we just return the integer
  provided.

  If atom does not currently exist, will warn that we're doing an unsafe conversion.
  """
  @spec maybe_to_atom(atom | String.t() | integer) :: atom | integer
  def maybe_to_atom(token) when is_atom(token), do: token

  def maybe_to_atom(token) when is_integer(token), do: token

  # We include a check for zero in this overload for 0 in case we have an integer
  # indexed map, the variable is still named snowflake for brevity.
  def maybe_to_atom(<<head, _rest::binary>> = token) when head in ?0..?9 do
    case Integer.parse(token) do
      {snowflake, ""} ->
        snowflake

      _ ->
        :erlang.binary_to_atom(token)
    end
  end

  def maybe_to_atom(token) do
    String.to_existing_atom(token)
  rescue
    _ ->
      Logger.debug(fn -> "Converting string to non-existing atom: #{token}" end)
      String.to_atom(token)
  end

  @doc """
  Converts possibly nil ISO8601 timestamp to a `DateTime`.

  If a `DateTime` is provided, return it as-is.
  """
  @spec maybe_to_datetime(String.t() | nil | DateTime.t()) :: DateTime.t() | nil
  def maybe_to_datetime(nil) do
    nil
  end

  def maybe_to_datetime(%DateTime{} = dt) do
    dt
  end

  def maybe_to_datetime(stamp) do
    {:ok, casted, 0} = DateTime.from_iso8601(stamp)
    casted
  end

  @doc """
  Converts possibly nil ISO8601 timestamp to unix time.
  """
  @spec maybe_to_unixtime(String.t() | nil) :: pos_integer() | nil
  def maybe_to_unixtime(nil) do
    nil
  end

  def maybe_to_unixtime(stamp) do
    stamp
    |> maybe_to_datetime()
    |> DateTime.to_unix()
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

  @doc """
  Updates a map with a new value if the key is present.
  Otherwise, returns the map unchanged.
  """
  @spec map_update_if_present(map(), term(), (term() -> term())) :: map()
  def map_update_if_present(map, key, fun) do
    case map do
      %{^key => value} ->
        new_value = fun.(value)
        Map.put(map, key, new_value)

      _ ->
        map
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
    Nostrum.Shard.Supervisor
    |> Supervisor.which_children()
    |> Enum.filter(fn {_id, _pid, _type, [modules]} -> modules == Nostrum.Shard end)
    |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.which_children(pid) end)
    |> List.flatten()
    |> Enum.map(fn {_id, pid, _type, _modules} -> Session.get_ws_state(pid) end)
    |> Enum.reduce(%{}, fn {_, s}, m -> Map.put(m, s.shard_num, get_shard_latency(s)) end)
  end

  @doc """
  Helper function for converting a DateTime to a Snowflake.
  While allowing Snowflakes to be returned as-is and `:infinity`
  is also passed through as-is since it's used as a special value
  by some cache lookup functions to indicate no upper bound.
  """
  def timestamp_like_to_snowflake(:infinity), do: :infinity
  def timestamp_like_to_snowflake(snowflake) when is_integer(snowflake), do: snowflake

  def timestamp_like_to_snowflake(%DateTime{} = dt) do
    case Snowflake.from_datetime(dt) do
      {:ok, snowflake} -> snowflake
      # The date we got was before Discord's epoch, so we'll just treat it as 0
      :error -> 0
    end
  end

  @doc """
  Since we're being sacrilegious and converting strings to atoms from the WS, there will be some
  atoms that we see that aren't defined in any Discord structs. This method mainly serves as a
  means to define those atoms once so the user isn't warned about them in the
  `Nostrum.Util.maybe_to_atom/1` function when they are in fact harmless.

  The function is public to prevent it from being optimized out at compile time.
  """
  def unused_atoms do
    [
      :active,
      :audio,
      :app_permissions,
      :audio_codec,
      :audio_ssrc,
      :burst,
      :channel_overrides,
      :convert_emoticons,
      :detect_platform_accounts,
      :developer_mode,
      :enable_tts_command,
      :encodings,
      :entitlement_sku_ids,
      :entitlements,
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
      :message_author_id,
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
