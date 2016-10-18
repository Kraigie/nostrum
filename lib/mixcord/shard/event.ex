defmodule Mixcord.Shard.Event do
  @moduledoc false

  alias Mixcord.Shard.Dispatch
  alias Mixcord.Shard.Payload
  alias Mixcord.Util
  require Logger

  def handle(:dispatch, payload, state) do
    # TODO: Task.start this? - when ratelimiter is implmented 100%
    Dispatch.handle(payload, state)

    state =
      if payload.t == :READY do
        %{state | session: payload.d.session_id}
      else
        state
      end

      %{state | reconnect_attempts: 0}
  end

  def handle(:heartbeat, _payload, state) do
    Logger.debug "GOT HEARTBEAT PING"
    :websocket_client.cast(self, {:binary, Payload.heartbeat_payload(state.seq)})
    state
  end

  def handle(:heartbeat_ack, _payload, state) do
    heartbeat_intervals = state.heartbeat_intervals
    |> List.delete_at(-1)
    |> List.insert_at(0, Util.now() - state.last_heartbeat)
    %{state | heartbeat_intervals: heartbeat_intervals}
  end

  def handle(:hello, payload, state) do
    # TODO: Check for resume if session is set, build resume packet and send - will also need to restart heartbeat
    heartbeat(self, payload.d.heartbeat_interval)
    identify(self)
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

  @doc false
  def heartbeat(pid, interval) do
    Process.send_after(pid, {:heartbeat, interval}, interval)
  end

  @doc false
  def identify(pid) do
    send(pid, :identify)
  end

end