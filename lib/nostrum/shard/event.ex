defmodule Nostrum.Shard.Event do
  @moduledoc false

  alias Nostrum.Shard.{Heartbeat, Payload}
  alias Nostrum.Shard.Stage.Producer
  alias Nostrum.Util

  require Logger

  def handle(:dispatch, payload, _conn, state) do
    payload = Util.safe_atom_map(payload)

    Logger.debug(payload.t)
    Producer.notify(Producer, payload, state)

    if payload.t == :READY do
      %{state | session: payload.d.session_id}
    else
      state
    end
  end

  def handle(:heartbeat, _payload, _conn, state) do
    Logger.info("HEARTBEAT PING")
    Heartbeat.send_heartbeat(state.gun_pid, state.seq)
    state
  end

  def handle(:heartbeat_ack, _payload, _conn, state) do
    Logger.info("HEARTBEAT_ACK")
    Heartbeat.ack(state.heartbeat_pid)
    state
  end

  def handle(:hello, payload, conn, state) do
    Heartbeat.start_loop(state.heartbeat_pid, payload.d.heartbeat_interval)

    if session_exists?(state) do
      Logger.info("RESUMING")
      resume(conn, state)
    else
      Logger.info("IDENTIFYING")
      identify(conn, state)
    end

    state
  end

  def handle(:invalid_session, _payload, conn, state) do
    Logger.info("INVALID_SESSION")
    identify(conn, state)
    state
  end

  def handle(:reconnect, _payload, _conn, state) do
    Logger.info("RECONNECT")
    state
  end

  def handle(event, _payload, _conn, state) do
    Logger.warn("UNHANDLED GATEWAY EVENT #{event}")
    state
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
