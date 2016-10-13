defmodule Mixcord.Rest.Ratelimiter do
  @moduledoc false

  use GenServer

  #Mixcord.Rest.Ratelimiter.create_bucket("TEST", 5, 5, 999)


  def start_link do
    GenServer.start_link(__MODULE__, [], name: Ratelimiter)
  end

  def init(_args) do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    {:ok, []}
  end

  def create_bucket(route, limit, remaining, reset) do
    :ets.insert_new(:ratelimit_buckets, {route, limit, remaining, reset})
  end

  def lookup_bucket(route) do
    :ets.lookup(:ratelimit_buckets, route)
  end

  def update_bucket(route, remaining) do
    :ets.update_element(:ratelimit_buckets, route, {3, remaining})
  end

  def delete_bucket(route) do
    :ets.delete(:ratelimit_buckets, route)
  end

  def handle_possible_ratelimit(route) do
    GenServer.call(Ratelimiter, {:check_limit, route}, :infinity)
  end

  def wait_for_limit(route, reset, from, state) do
    IO.inspect "IN WAIT FOR LIMIT"
    IO.inspect("ABOUT TO SEND AFTER")
    Process.send_after(self, {:reply, route, from}, reset)
  end

  def handle_info({:reply, route, from}, state) do
    IO.inspect("HANDLE INFO BBY, GONNA CHECK RATELIMIT AGAIN")
    case rate_limited?(route) do
      {:ratelimited, reset} ->
        {:noreply, wait_for_limit(route, reset, from, state)}
      {:ok} ->
        {:noreply, state}
    end
    delete_bucket(route)
    IO.inspect("ABOUT TO SEND REPLY")
    GenServer.reply(from, :ok)
    IO.inspect("JUST SEND REPLY")
    {:noreply, state}
  end

  def handle_call({:check_limit, route}, from, state) do
    case rate_limited?(route) do
      {:ratelimited, reset} ->
        {:noreply, wait_for_limit(route, reset, from, state)}
      {:ok} ->
        {:reply, :ok, state}
    end
  end

  def rate_limited?(route) do
    case lookup_bucket(route) do
      [] ->
        {:ok}
      [{route, limit, remaining, reset}] when remaining <= 1 ->
        update_bucket(route, remaining - 1)
        {:ratelimited, reset}
      [{route, limit, remaining, reset}] ->
        update_bucket(route, remaining - 1)
        {:ok}
    end
  end

end