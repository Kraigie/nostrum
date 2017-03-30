defmodule Nostrum.Shard.Event do
  @moduledoc false

  alias Nostrum.Shard.{Dispatch, Payload}
  alias Nostrum.Util

  require Logger

  def handle(:dispatch, payload, state) do
    [{pid, _id}] = Registry.lookup(ProducerRegistry, state.shard_num)
    payload = Util.safe_atom_map(payload)

    Task.Supervisor.start_child(DispatchTaskSupervisor, fn ->
      Dispatch.handle(pid, payload, state)
    end)

    state =
      if payload.t == :READY do
        %{state | session: payload.d.session_id}
      else
        state
      end

      %{state | reconnect_attempts: 0}
  end

  def handle(:heartbeat, _payload, state) do
    Logger.debug "HEARTBEAT PING"
    :websocket_client.cast(self(), {:binary, Payload.heartbeat_payload(state.seq)})
    state
  end

  def handle(:heartbeat_ack, _payload, state) do
    Logger.debug "HEARTBEAT_ACK"
    %{state | heartbeat_ack: true}
  end

  def handle(:hello, payload, state) do
    if session_exists?(state) do
      Logger.debug "RESUMING"
      resume(self())
    else
      Logger.debug "IDENTIFYING"
      identify(self())
    end

    pid = self()
    heartbeat_task = Task.async(fn -> heartbeat(pid, payload.d.heartbeat_interval) end)

    %{state | heartbeat_task: heartbeat_task}
  end

  def handle(:invalid_session, _payload, state) do
    Logger.debug "INVALID_SESSION"
    identify(self())
    state
  end

  def handle(:reconnect, _payload, state) do
    Logger.debug "RECONNECT"
    state
  end

  def handle(event, _payload, state) do
    Logger.warn "UNHANDLED GATEWAY EVENT #{event}"
    state
  end

  def heartbeat(pid, interval) do
    Process.sleep(interval)
    send(pid, :heartbeat)
    heartbeat(pid, interval)
  end

  def identify(pid) do
    send(pid, :identify)
  end

  def resume(pid) do
    send(pid, :resume)
  end

  def session_exists?(state) do
    not is_nil(state.session)
  end

end
