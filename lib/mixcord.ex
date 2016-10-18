defmodule Mixcord do
  @moduledoc """
  """

  use Application

  def start(_, _) do
    import Supervisor.Spec

    token = Application.get_env(:mixcord, :token)
    caller = Application.get_env(:mixcord, :caller)
    num_shards = Application.get_env(:mixcord, :num_shards)

    setup_ets_tables

    children = [
      supervisor(Mixcord.Cache.Supervisor, []),
      supervisor(Mixcord.Shard.Supervisor, [token, caller, num_shards])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def setup_ets_tables do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    :ets.new(:gateway_url, [:set, :public, :named_table])
  end

  # allows us to run mixcord as a standalone
  def handle_event(_data, _state), do: :ok

end

defmodule Test do
  use GenServer
  def start_link do
    GenServer.start_link(__MODULE__, [], name: TEST)
  end

  def add(num) do
    IO.inspect("CALLING ADD")
    GenServer.call(TEST, {:add, num}, :infinity)
  end

  def handle_call({:add, num}, from, state) do
    IO.inspect("HANDLING ADD FOR #{num}")
    #check ratelimit
    if num == 1 do
      #not ratelimited
      IO.inspect("sleeping to simulate api call")
      Process.sleep(3000)
      #update ratelimits
      GenServer.reply(from, "api response")
    else
      #ratelimited
      Task.start(fn -> wait_for_timeout(10000) end)
    end


    {:noreply, state}
  end

  def wait_for_timeout(time) do
    IO.inspect("ABOUT TO SLEEP")
    Process.sleep(time)
    IO.inspect("CALLING ADD FROM WAIT TIMEOUT")
    GenServer.call(TEST, {:add, 1}, :infinity)
  end


  """
  def handle_call(:TEST, _from, state) do
    Process.sleep(3000)
    IO.inspect("DONE SLEEPING")
    {:reply, "GOT IT", state}
  end


  def handle_call(:add, from, state) do
    get_ratelimit_time
    if rate_limit_time do
      Process.send_after(self, {ratelimit_over}, ratelimit_time)
    end

    make_rest_call
    update_ratelimits

    GenServer.reply(from, from_rest_call)

    {:noreply, state}
  end


  def handle_info({:test, message, from}, state) do
    IO.inspect("HANDLING INFO")
    GenServer.reply(from, message)
    {:noreply, state}
  end

  def handle_call(:OTHER, from, state) do
    IO.inspect(from)
    IO.inspect("DOING OTHER")
    Process.send_after(self, {:test, "HELLO WORLD", from}, 3000)
    {:noreply, state}
  end
  """
end
