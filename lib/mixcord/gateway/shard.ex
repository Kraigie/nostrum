defmodule Mixcord.Shard do
  @moduledoc false

  alias Mixcord.Constants
  import Mixcord.Shard.Helpers

  @behaviour :websocket_client

  #hide sharding
  def start_link(token, caller) do
    start_link(token, caller, 1)
  end

  #expose sharding
  def start_link(token, caller, shard_num) do
    :crypto.start
    :ssl.start
    state_map = Map.new([token: token, shard_num: shard_num, caller: caller, seq: nil, reconnect_attempts: 0])
    :websocket_client.start_link(gateway() , __MODULE__, state_map)
  end

  def websocket_handle({:binary, payload}, _state, state_map) do
    payload = :erlang.binary_to_term(payload)
    case Constants.name_from_opcode payload.op do
      "DISPATCH" ->
        handle_event(payload, state_map)
        #state_map.caller.handle_event({event, payload}, other, stuff) to run users commands
      "HELLO" ->
        #TODO: Check for resume
        heartbeat(self, payload.d.heartbeat_interval)
        identify(self, state_map)
      "HEARTBEAT_ACK" ->
        IO.inspect "GOT ACK"
    end

    #TODO: Find somewhere else to do this probably
    {:ok, %{state_map | reconnect_attempts: 0}}
  end

  def init(state_map) do
    {:once, state_map}
  end

  def onconnect(_ws_req, state_map) do
    {:ok, state_map}
  end

  def ondisconnect(reason, state_map) do
    if state_map.reconnect_attempts > 2 do
      {:close, reason, state_map}
    else
      :timer.sleep(15000)
      attempts = state_map.reconnect_attempts + 1
      IO.inspect "RECONNECT ATTEMPT NUMBER #{attempts}"
      {:reconnect, %{state_map | reconnect_attempts: attempts}}
    end
  end

  def websocket_info({:status_update, new_status}, _ws_req, state_map) do
    #TODO: Flesh this out
    :websocket_client.cast(self, {new_status})
    {:ok, state_map}
  end

  def websocket_info({:heartbeat, interval}, _ws_req, state_map) do
    :websocket_client.cast(self, {:binary, heartbeat_payload(state_map.seq)})
    heartbeat(self, interval)
    {:ok, state_map}
  end

  def websocket_terminate(reason, _ws_req, _state_map) do
    #SO TRIGGERED I CANT GET END EVENT CODES
    IO.inspect "WS TERMINATED BECAUSE: #{reason}"
    :ok
  end

end