defmodule Mixcord.Util do
  @moduledoc false

  alias Mixcord.{Api, Constants}

  def empty_cache do
    Mixcord.Cache.Supervisor.empty_cache
  end

  def now do
    DateTime.utc_now
      |> DateTime.to_unix(:milliseconds)
  end

  def num_shards do
    Application.get_env(:mixcord, :num_shards)
  end

  def bangify_find(to_bang, find, cache_name) do
    case to_bang do
      nil ->
        raise(Mixcord.Error.CacheError, finding: find, cache_name: cache_name)
      ret ->
        ret
    end
  end

  def gateway do
    case :ets.lookup(:gateway_url, "url") do
      [] -> get_new_gateway_url()
      [{"url", url}] -> url
    end
  end

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

  def safe_atom_map(term) do
    cond do
      is_map(term) -> for {key, value} <- term, into: %{}, do: {maybe_to_atom(key), safe_atom_map(value)}
      is_list(term) -> Enum.map(term, fn item -> safe_atom_map(item) end)
      true -> term
    end
  end

  def maybe_to_atom(token) when is_atom(token), do: token
  # TODO: FINISH THE MAPS CRAIG
  # true -> String.to_existing_atom(token)
  def maybe_to_atom(token), do: String.to_atom(token)

end