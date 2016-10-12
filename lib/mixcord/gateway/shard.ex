defmodule Mixcord.Shard do
  @moduledoc false

  import Mixcord.Shard.Helpers
  alias Mixcord.Constants
  require Logger

  @behaviour :websocket_client

  def start_link(token, caller, shard_num) do
    :crypto.start
    :ssl.start
    :websocket_client.start_link(gateway, __MODULE__, state_map(token, caller, shard_num)
  end

  def websocket_handle({:binary, payload}, _state, state_map) do
    payload = :erlang.binary_to_term(payload)
    new_state =
      case Constants.name_from_opcode payload.op do
        "DISPATCH" ->
          #TODO: Task.Async this?
          handle_event(payload, state_map)
          %{state_map | reconnect_attempts: 0}
        "HELLO" ->
          #TODO: Check for resume
          heartbeat(self, payload.d.heartbeat_interval)
          identify(self)
          state_map
        "HEARTBEAT_ACK" ->
          heartbeat_intervals = state_map.heartbeat_intervals
            |> List.delete_at(-1)
            |> List.insert_at(0, state_map.last_heartbeat - now())
          %{state_map | heartbeat_intervals: heartbeat_intervals}
      end
    {:ok, new_state}
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
    Logger.debug "CONNECTED ON #{state_map.shard_num}"
    {:ok, state_map}
  end

  def ondisconnect(reason, state_map) do
    Logger.debug "WS DISCONNECTED BECAUSE: #{inspect reason}"
    cond do
      state_map.reconnect_attempts > 2 ->
        {:close, reason, state_map}
      true ->
        :timer.sleep(15_000)
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