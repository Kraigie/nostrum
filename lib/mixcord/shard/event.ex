defmodule Mixcord.Shard.Event do
  @moduledoc false

  alias Mixcord.Shard.{Dispatch, Payload}
  alias Mixcord.Util
  require Logger

  def handle(:dispatch, payload, state) do
    Task.start(fn -> Dispatch.handle(payload, state) end)

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
    :websocket_client.cast(self, {:binary, Payload.heartbeat_payload(state.seq)})
    state
  end

  def handle(:heartbeat_ack, _payload, state) do
    Logger.debug "HEARTBEAT_ACK"
    heartbeat_intervals = state.heartbeat_intervals
    |> List.delete_at(-1)
    |> List.insert_at(0, Util.now() - state.last_heartbeat)
    %{state | heartbeat_intervals: heartbeat_intervals}
  end

  def handle(:hello, payload, state) do
    if session_exists?(state) do
      resume(self)
    else
      identify(self)
    end

    # TODO: Remove duplicate heartbeat after resuming (or any other :hello messages).
    # Likely want to move heartbeat to its own process that we can kill. Will need a process per shard.
    # Will need to store heartbeat interval?
    heartbeat(self, payload.d.heartbeat_interval)
    state
  end

  def handle(:invalid_session, _payload, state) do
    Logger.debug "INVALID_SESSION"
    identify(self)
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
    Process.send_after(pid, {:heartbeat, interval}, interval)
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