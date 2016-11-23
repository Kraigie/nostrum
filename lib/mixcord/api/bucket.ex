defmodule Mixcord.Api.Bucket do
  @moduledoc false

  alias Mixcord.Util

  def create_bucket(route, remaining, reset_time, latency) do
    :ets.insert(:ratelimit_buckets, {route, remaining, reset_time, latency})
  end

  def lookup_bucket(route) do
    route_time = :ets.lookup(:ratelimit_buckets, route)
    global_time = :ets.lookup(:ratelimit_buckets, "GLOBAL")

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
      [{route, remaining, reset_time, latency}] when remaining <= 0 ->
        update_bucket(route, remaining - 1)
        wait_time = reset_time - Util.now + latency
        if wait_time <= 0 do
          delete_bucket(route)
          nil
        else
          wait_time
        end
      [{route, remaining, _reset_time, latency}] ->
        update_bucket(route, remaining - 1)
        nil
    end
  end

end