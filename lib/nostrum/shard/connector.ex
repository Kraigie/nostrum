defmodule Nostrum.Shard.Connector do
  @moduledoc false

  use GenServer

  alias Nostrum.Util

  require Logger

  @wait_time 5500

  def start_link([]) do
    GenServer.start_link(__MODULE__, %{last_connect: 0}, name: __MODULE__)
  end

  def init(args) do
    {:ok, args}
  end

  def block_until_connect do
    GenServer.call(__MODULE__, {:blocking_connect}, :infinity)
  end

  def handle_call({:blocking_connect}, _from, %{last_connect: 0} = state), do: wait(0, state)

  def handle_call({:blocking_connect}, _from, state) do
    time_to_wait = Util.now() - state.last_connect
    wait(time_to_wait, state)
  end

  def wait(_time, %{last_connect: 0} = state),
    do: {:reply, :ok, %{state | last_connect: Util.now()}}

  def wait(time, state) when time >= @wait_time,
    do: {:reply, :ok, %{state | last_connect: Util.now()}}

  def wait(time, state) do
    Logger.info("WAITING #{@wait_time - time} BEFORE NEXT SHARD CONNECT")
    Process.sleep(@wait_time - time)
    {:reply, :ok, %{state | last_connect: Util.now()}}
  end

  def code_change(_version, state, _extra) do
    {:ok, state}
  end
end
