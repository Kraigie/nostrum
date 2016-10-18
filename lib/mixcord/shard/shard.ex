defmodule Mixcord.Shard do
  @moduledoc false

  import Mixcord.Shard.Helpers
  alias Mixcord.Constants
  require Logger

  @behaviour :websocket_client
  @num_shards Application.get_env(:mixcord, :num_shards)

  def start_link(token, caller, shard_num) do
    :crypto.start
    :ssl.start
    # This makes the supervisor spawn a shard worker ever 5 seconds. This only occurs on ShardSupervisor start.
    # If an individual shard fails, it will be restarted immediately.
    # TODO: Queue reconnects/check this better
    if @num_shards > 1, do: Process.sleep(5000)
    :websocket_client.start_link(gateway, __MODULE__, state_map(token, caller, shard_num))
  end

  def websocket_handle({:binary, payload}, _state, state) do
    payload = :erlang.binary_to_term(payload)
    new_state =
      case Constants.name_from_opcode payload.op do
        "DISPATCH" ->
          #TODO: Task.start this?
          handle_dispatch(payload, state)
          state =
            if payload.t == :READY do
              %{state | session: payload.d.session_id}
            else
              state
            end

          %{state | reconnect_attempts: 0}
        "HEARTBEAT" ->
          Logger.debug "GOT HEARTBEAT PING"
          :websocket_client.cast(self, {:binary, heartbeat_payload(state.seq)})
          state
        "HEARTBEAT_ACK" ->
          heartbeat_intervals = state.heartbeat_intervals
            |> List.delete_at(-1)
            |> List.insert_at(0, state.last_heartbeat - now())
          %{state | heartbeat_intervals: heartbeat_intervals}
        "HELLO" ->
          #TODO: Check for resume if session is set, build resume packet and send - will also need to restart heartbeat
          heartbeat(self, payload.d.heartbeat_interval)
          identify(self)
          state
        "INVALID_SESSION" ->
          Logger.debug "INVALID_SESSION"
          identify(self)
          state
        "RECONNECT" ->
          Logger.debug "RECONNECT"
          state
      end
    {:ok, %{new_state | seq: payload.s}}
  end

  def websocket_info({:status_update, new_status_json}, _ws_req, state) do
    #TODO: Flesh this out - Idle time?
    :websocket_client.cast(self, {:binary, status_update_payload(new_status_json)})
    {:ok, state}
  end

  def websocket_info({:heartbeat, interval}, _ws_req, state) do
    now = now()
    :websocket_client.cast(self, {:binary, heartbeat_payload(state.seq)})
    heartbeat(self, interval)
    {:ok, %{state | last_heartbeat: now}}
  end

  def websocket_info(:identify, _ws_req, state) do
    :websocket_client.cast(self, {:binary, identity_payload(state)})
    {:ok, state}
  end

  def init(state) do
    {:once, state}
  end

  def onconnect(_ws_req, state) do
    Logger.debug "SHARD #{state.shard_num} CONNECTED"
    {:ok, state}
  end

  def ondisconnect(reason, state) do
    Logger.debug "WS DISCONNECTED BECAUSE: #{inspect reason}"
    Logger.debug "STATE ON CLOSE: #{inspect state}"
    if state.reconnect_attempts > 3 do
      {:close, reason, state}
    else
      :timer.sleep(5000)
      Logger.debug "RECONNECT ATTEMPT NUMBER #{state.reconnect_attempts + 1}"
      {:reconnect, %{state | reconnect_attempts: state.reconnect_attempts + 1}}
    end
  end

  def websocket_terminate(reason, _ws_req, _state) do
    #SO TRIGGERED I CANT GET END EVENT CODES
    Logger.debug "WS TERMINATED BECAUSE: #{inspect reason}"
    :ok
  end

end