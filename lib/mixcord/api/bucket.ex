defmodule Mixcord.Api.Bucket do
  @moduledoc false

  alias Mixcord.Util

  def create_bucket(route, limit, remaining, reset) do
    :ets.insert(:ratelimit_buckets, {route, limit, remaining, reset})
  end

  def lookup_bucket(route) do
    route_time = :ets.lookup(:ratelimit_buckets, route)
    global_time = :ets.lookup(:ratelimit_buckets, "GLOBAL")

    Enum.max([route_time, global_time])
  end

  def update_bucket(route, remaining) do
    :ets.update_element(:ratelimit_buckets, route, {3, remaining})
  end

  def delete_bucket(route) do
    :ets.delete(:ratelimit_buckets, route)
  end

  def get_ratelimit_timeout(route) do
    case lookup_bucket(route) do
      [] ->
        nil
      [{route, _limit, remaining, reset}] when remaining <= 0 ->
        update_bucket(route, remaining - 1)
        wait_time = reset - Util.now()

        if wait_time <= 0 do
          delete_bucket(route)
          nil
        else
          wait_time
        end
      [{route, _limit, remaining, _reset}] ->
        update_bucket(route, remaining - 1)
        nil
    end
  end

end