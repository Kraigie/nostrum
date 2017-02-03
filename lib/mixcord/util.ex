defmodule Mixcord.Util do
  @moduledoc """
  Utility functions
  """

  alias Mixcord.{Api, Constants}
  require Logger

  @doc """
  Empties all caches.
  """
  @spec empty_cache() :: no_return
  def empty_cache do
    Mixcord.Cache.Supervisor.empty_cache
  end

  @doc """
  Returns the number of milliseconds since unix epoch.
  """
  @spec now() :: Integer.t
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

  @doc """
  Returns the number of shards.

  This is not the number of currently active shards, but the number of shards specified
  in your config.
  """
  @spec num_shards() :: Integer.t
  def num_shards do
    Application.get_env(:mixcord, :num_shards)
  end

  @doc false
  def bangify_find(to_bang, find, cache_name) do
    case to_bang do
      nil ->
        raise(Mixcord.Error.CacheError, finding: find, cache_name: cache_name)
      ret ->
        ret
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
      [{"url", url}] -> url
    end
  end

  @doc false
  defp get_new_gateway_url do
    case Api.request(:get, Constants.gateway, "") do
      {:error, %{status_code: code, message: message}} ->
        raise(Mixcord.Error.ApiError, status_code: code, message: message)
      {:ok, body} ->
        body = Poison.decode!(body)
        url = body["url"] <> "?encoding=etf&v=6"
          |> to_charlist
        :ets.insert(:gateway_url, {"url", url})
        url
    end
  end

  @doc """
  Converts a map into an atom-keyed map.

  Given a map with variable type keys, returns the same map with all keys at `atoms`.

  This function will attempt to convert keys to an existing atom, and if that fails will default to
  creating a new atom while displaying a warning. The idea here is that we should be able to see
  if any results from Discord are giving variable keys. Since we *will* define all
  types of objects returned by Discord, the amount of new atoms created *SHOULD* be 0. 👀
  """
  @spec safe_atom_map(Map.t) :: Map.t
  def safe_atom_map(term) do
    cond do
      is_map(term) -> for {key, value} <- term, into: %{}, do: {maybe_to_atom(key), safe_atom_map(value)}
      is_list(term) -> Enum.map(term, fn item -> safe_atom_map(item) end)
      true -> term
    end
  end

  @doc false
  def maybe_to_atom(token) when is_atom(token), do: token
  def maybe_to_atom(token) do
    try do
      # TODO: FINISH THE MAPS CRAIG
      String.to_atom(token)
      # String.to_existing_atom(token)
    rescue
      error ->
        Logger.warn "Converting string to non-existing atom: #{token}"
        String.to_atom(token)
    end
  end

end