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
    state_map = Map.new([token: token, shard_num: shard_num, caller: caller])
    :websocket_client.start_link(gateway() , __MODULE__, state_map)
  end

  def websocket_handle({:binary, payload}, _state, state_map) do
    payload = :erlang.binary_to_term(payload)

    case Constants.name_from_opcode payload.op do
      "EVENT" ->
        handle_event()
      "HELLO" ->
        identify()
      #state_map.caller.do_something(msg) to run users commands
    end

    {:ok, state_map}
  end

  def init(state_map) do
    {:once, state_map}
  end

  def onconnect(_ws_req, state_map) do
    #identify :websocket_client.cast(self, {:text, "message"})
    {:ok, state_map}
  end

  def ondisconnect(reason, state) do
    {:close, reason, state}
  end

  def websocket_info({:status_update, new_status}, _ws_req, state) do
    #TODO: Flesh this out
    :websocket_client.cast(self, {new_status})
    {:ok, state}
  end

  def websocket_terminate(_close_info, _ws_req, _state) do
    :ok
  end

end