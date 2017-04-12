defmodule Nostrum.Shard.Event do
  @moduledoc false

  alias Nostrum.Shard.{Dispatch, Payload}
  alias Nostrum.Util

  require Logger

  def handle(:dispatch, payload, _conn, state) do
    [{pid, _id}] = Registry.lookup(ProducerRegistry, state.shard_num)
    payload = Util.safe_atom_map(payload)

    Task.Supervisor.start_child(DispatchTaskSupervisor, fn ->
      Dispatch.handle(pid, payload, state)
    end)

    if payload.t == :READY do
      %{state | session: payload.d.session_id}
    else
      state
    end
  end

  def handle(:heartbeat, _payload, conn, state) do
    Logger.debug "HEARTBEAT PING"
    gun_send(conn, Payload.heartbeat_payload(state.seq))
    state
  end

  def handle(:heartbeat_ack, _payload, _conn, state) do
    Logger.debug "HEARTBEAT_ACK"
    %{state | heartbeat_ack: true}
  end

  def handle(:hello, payload, conn, state) do
    if session_exists?(state) do
      Logger.debug "RESUMING"
      resume(conn, state)
    else
      Logger.debug "IDENTIFYING"
      identify(conn, state)
      heartbeat(conn, payload.d.heartbeat_interval)
    end

    state
  end

  def handle(:invalid_session, _payload, conn, state) do
    Logger.debug "INVALID_SESSION"
    identify(conn, state)
    state
  end

  def handle(:reconnect, _payload, _conn, state) do
    Logger.debug "RECONNECT"
    state
  end

  def handle(event, _payload, _conn, state) do
    Logger.warn "UNHANDLED GATEWAY EVENT #{event}"
    state
  end

  def heartbeat(conn, interval) do
    Process.send_after(self(), {:heartbeat, interval, conn}, interval)
  end

  def identify(conn, state) do
    gun_send(conn, Payload.identity_payload(state))
  end

  def resume(conn, state) do
    gun_send(conn, Payload.resume_payload(state))
  end

  def session_exists?(state) do
    not is_nil(state.session)
  end

  def gun_send(conn, payload) do
    :gun.ws_send(conn, {:binary, payload})
  end
end
