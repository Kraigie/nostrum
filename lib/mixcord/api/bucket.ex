defmodule Mixcord.Api.Bucket do
  @moduledoc false

  alias Mixcord.Util

  def create_bucket(route, remaining, retry_after) do
    :ets.insert(:ratelimit_buckets, {route, remaining, retry_after})
  end

  def lookup_bucket(route) do
    IO.inspect route_time = :ets.lookup(:ratelimit_buckets, route)
    IO.inspect global_time = :ets.lookup(:ratelimit_buckets, "GLOBAL")

    Enum.max([route_time, global_time])
  end

  def update_bucket(route, remaining) do
    :ets.update_element(:ratelimit_buckets, route, {2, remaining})
  end

  def delete_bucket(route) do
    :ets.delete(:ratelimit_buckets, route)
  end

  def get_ratelimit_timeout(route) do
    case lookup_bucket(route) do
      [] ->
        nil
      [{route, remaining, retry_after}] when remaining <= 0 ->
        update_bucket(route, remaining - 1)
        delete_bucket(route)
        retry_after * 1000
      [{route, remaining, _retry_after}] ->
        update_bucket(route, remaining - 1)
        nil
    end
  end

end