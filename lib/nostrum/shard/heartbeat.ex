defmodule Nostrum.Shard.Heartbeat do
  @moduledoc false

  use GenServer

  require Logger

  def send_heartbeat(pid, seq) do
    send(pid, {:send_heartbeat, seq})
  end

  def force_disconnect(pid) do
    send(pid, :force_disconnect)
  end

  def start_link() do
    GenServer.start_link(__MODULE__, [])
  end

  def update_sequence(pid, seq) do
    GenServer.cast(pid, {:update_seq, seq})
  end

  def update_gun_onwer(pid, owner_pid) do
    GenServer.cast(pid, {:update_gun_owner, owner_pid})
  end

  def ack(pid) do
    GenServer.cast(pid, :ack_received)
  end

  def start_loop(pid, interval) do
    send(pid, {:loop, interval})
  end

  def init(_opts) do
    {:ok, %{pid: nil, seq: 0, ack_received: true, timer: nil}}
  end

  def handle_cast({:update_seq, seq}, state) do
    {:noreply, %{state | seq: seq}}
  end

  def handle_cast({:update_gun_owner, pid}, state) do
    {:noreply, %{state | pid: pid}}
  end

  def handle_cast(:ack_received, state) do
    {:noreply, %{state | ack_received: true}}
  end

  def handle_info({:loop, interval}, %{ack_received: true} = state) do
    send_heartbeat(state.pid, state.seq)
    timer = Process.send_after(self(), {:loop, interval}, interval)
    {:noreply, %{state | timer: timer, ack_received: false}}
  end

  def handle_info({:loop, _interval}, %{ack_received: false} = state) do
    Logger.warn("HEARTBEAT_ACK not received in time, disconnecting")
    force_disconnect(state.pid)
    {:noreply, state}
  end
end
