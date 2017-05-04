defmodule Nostrum.Util do
  @moduledoc """
  Utility functions
  """

  alias Nostrum.{Api, Constants}
  alias Nostrum.Cache.CacheSupervisor

  require Logger

  @doc """
  Helper for defining all the methods used for struct and encoding transformations.

  ## Example
  ``` Elixir
  Nostrum.Util.nostrum_struct([
    author: User,
    mentions: [User],
    mention_roles: [User],
    embeds: [Embed]
  ])
  ```
  """
  defmacro nostrum_struct(body) do
    quote do
      @derive [Poison.Encoder]
      defstruct Map.keys(unquote(body))

      def p_encode do
        encoded = for {k, v} <- unquote(body), v != nil, into: %{} do
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
          for {k, v} <- unquote(body), v != nil, into: %{} do
            case v do
              [v] -> {k, Util.list_to_struct_list(Map.get(map, k), v)}
              v -> {k, apply(v, :to_struct, [Map.get(map, k)])}
            end
          end
        struct(__ENV__.module, new_map)
      end
    end
  end

  @doc """
  Empties all caches.
  """
  @spec empty_cache() :: no_return
  def empty_cache do
    CacheSupervisor.empty_cache
  end

  @doc """
  Returns the number of milliseconds since unix epoch.
  """
  @spec now() :: integer
  def now do
    DateTime.utc_now
    |> DateTime.to_unix(:milliseconds)
  end

  @doc """
  Returns the current date as an ISO formatted string.
  """
  @spec now_iso() :: String.t
  def now_iso do
    DateTime.utc_now
    |> DateTime.to_iso8601
  end

  @doc false
  def list_to_struct_list(list, struct) do
    Enum.map(list, &struct.to_struct(&1))
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
  Returns the gateway url for current websocket connections.

  If by chance no gateway connection has been made, will fetch the url to use and store it
  for future use.
  """
  @spec gateway() :: String.t
  def gateway do
    case :ets.lookup(:gateway_url, "url") do
      [] -> get_new_gateway_url()
      [{"url", url, shards}] -> {url, shards}
    end
  end

  defp execute_gateway_request(true),
    do: Api.request(:get, Constants.gateway, "")
  defp execute_gateway_request(nil),
    do: Api.request(:get, Constants.gateway_bot, "")
  defp execute_gateway_request(false),
    do: Api.request(:get, Constants.gateway_bot, "")

  defp get_new_gateway_url do
    case execute_gateway_request(Application.get_env(:nostrum, :self_bot)) do
      {:error, %{status_code: code, message: message}} ->
        raise(Nostrum.Error.ApiError, status_code: code, message: message)
      {:ok, body} ->
        body = Poison.decode!(body)

        url = body["url"]
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
      is_map(term) -> for {key, value} <- term, into: %{}, do: {maybe_to_atom(key), safe_atom_map(value)}
      is_list(term) -> Enum.map(term, fn item -> safe_atom_map(item) end)
      true -> term
    end
  end

  @doc """
  Attempts to convert a string to an atom.

  If atom does not currently exist, will warn that we're doing an unsafe conversion.
  """
  @spec maybe_to_atom(atom | String.t) :: atom
  def maybe_to_atom(token) when is_atom(token), do: token
  def maybe_to_atom(token) do
    String.to_existing_atom(token)
  rescue
    _ ->
      Logger.debug "Converting string to non-existing atom: #{token}"
      String.to_atom(token)
  end

  @doc """
  Since we're being sacrilegious and converting strings to atoms from the WS, there will be some
  atoms that we see that aren't defined in any Discord structs. This method mainly serves as a
  means to define those atoms once so the user isn't warned about them in the
  `Nostrum.Util.maybe_to_atom/1` function when they are in fact harmless.
  """
  def unused_atoms do
    [
      recipients: "Ready", require_colons: "Ready", last_message_id: "Ready",
      friend_sync: "Self", visibility: "Self", channel_overrides: "Self",
      message_notifications: "Self", muted: "Self", mobile_push: "Self",
      suppress_everyone: "Self", convert_emoticons: "Self",
      detect_platform_accounts: "Self", developer_mode: "Self",
      enable_tts_command: "Self", friend_source_flags: "Self",
      guild_positions: "Self", inline_attachment_media: "Self",
      inline_embed_media: "Self", locale: "Self", message_display_compact: "Self",
      render_embeds: "Self", render_reactions: "Self", restricted_guilds: "Self",
      show_current_game: "Self", theme: "Self",
    ]
  end

end
