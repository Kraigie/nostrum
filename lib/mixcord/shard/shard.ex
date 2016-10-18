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

  def websocket_handle({:binary, payload}, _state, state_map) do
    payload = :erlang.binary_to_term(payload)
    new_state =
      case Constants.name_from_opcode payload.op do
        "DISPATCH" ->
          #TODO: Task.start this?
          handle_dispatch(payload, state_map)
          state_map =
            if payload.t == :READY do
              %{state_map | session: payload.d.session_id}
            else
              state_map
            end

          %{state_map | reconnect_attempts: 0}
        "HEARTBEAT" ->
          Logger.debug "GOT HEARTBEAT PING"
          :websocket_client.cast(self, {:binary, heartbeat_payload(state_map.seq)})
          state_map
        "HEARTBEAT_ACK" ->
          heartbeat_intervals = state_map.heartbeat_intervals
            |> List.delete_at(-1)
            |> List.insert_at(0, state_map.last_heartbeat - now())
          %{state_map | heartbeat_intervals: heartbeat_intervals}
        "HELLO" ->
          #TODO: Check for resume if session is set, build resume packet and send - will also need to restart heartbeat
          heartbeat(self, payload.d.heartbeat_interval)
          identify(self)
          state_map
        "INVALID_SESSION" ->
          Logger.debug "INVALID_SESSION"
          identify(self)
          state_map
        "RECONNECT" ->
          Logger.debug "RECONNECT"
          state_map
      end
    {:ok, %{new_state | seq: payload.s}}
  end

  def websocket_info({:status_update, new_status_json}, _ws_req, state_map) do
    #TODO: Flesh this out - Idle time?
    :websocket_client.cast(self, {:binary, status_update_payload(new_status_json)})
    {:ok, state_map}
  end

  def websocket_info({:heartbeat, interval}, _ws_req, state_map) do
    now = now()
    :websocket_client.cast(self, {:binary, heartbeat_payload(state_map.seq)})
    heartbeat(self, interval)
    {:ok, %{state_map | last_heartbeat: now}}
  end

  def websocket_info(:identify, _ws_req, state_map) do
    :websocket_client.cast(self, {:binary, identity_payload(state_map)})
    {:ok, state_map}
  end

  def init(state_map) do
    {:once, state_map}
  end

  def onconnect(_ws_req, state_map) do
    Logger.debug "SHARD #{state_map.shard_num} CONNECTED"
    {:ok, state_map}
  end

  def ondisconnect(reason, state_map) do
    Logger.debug "WS DISCONNECTED BECAUSE: #{inspect reason}"
    Logger.debug "STATE ON CLOSE: #{inspect state_map}"
    if state_map.reconnect_attempts > 3 do
      {:close, reason, state_map}
    else
      :timer.sleep(5000)
      Logger.debug "RECONNECT ATTEMPT NUMBER #{state_map.reconnect_attempts + 1}"
      {:reconnect, %{state_map | reconnect_attempts: state_map.reconnect_attempts + 1}}
    end
  end

  def websocket_terminate(reason, _ws_req, _state_map) do
    #SO TRIGGERED I CANT GET END EVENT CODES
    Logger.debug "WS TERMINATED BECAUSE: #{inspect reason}"
    :ok
  end

end