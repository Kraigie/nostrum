defmodule Nostrum.Shard.Event do
  @moduledoc false

  alias Nostrum.Shard.Payload
  alias Nostrum.Shard.Stage.Producer
  alias Nostrum.Util

  require Logger

  def handle(:dispatch, payload, state) do
    payload = Util.safe_atom_map(payload)

    if Application.get_env(:nostrum, :log_dispatch_events), do: Logger.debug(payload.t)
    Producer.notify(Producer, payload, state)

    if payload.t == :READY do
      %{state | session: payload.d.session_id}
    else
      state
    end
  end

  def handle(:heartbeat, _payload, state) do
    Logger.info("HEARTBEAT PING")
    {state, Payload.heartbeat_payload(state.seq)}
  end

  def handle(:heartbeat_ack, _payload, state) do
    Logger.info("HEARTBEAT_ACK")
    %{state | last_heartbeat_ack: DateTime.utc_now(), heartbeat_ack: true}
  end

  def handle(:hello, payload, state) do
    state = %{state | heartbeat_interval: payload.d.heartbeat_interval}

    WebSockex.cast(state.conn_pid, :heartbeat)

    if session_exists?(state) do
      Logger.info("RESUMING")
      {state, Payload.resume_payload(state)}
    else
      Logger.info("IDENTIFYING")
      {state, Payload.identity_payload(state)}
    end
  end

  def handle(:invalid_session, _payload, state) do
    Logger.info("INVALID_SESSION")
    {state, Payload.identity_payload(state)}
  end

  def handle(:reconnect, _payload, state) do
    Logger.info("RECONNECT")
    state
  end

  def handle(event, _payload, state) do
    Logger.warn("UNHANDLED GATEWAY EVENT #{event}")
    state
  end

  def session_exists?(state) do
    not is_nil(state.session)
  end
end
