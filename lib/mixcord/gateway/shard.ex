defmodule Mixcord.Shard do
  @moduledoc """
  """

  alias Mixcord.Constants
  alias Mixcord.Rest.Client

  @behaviour :websocket_client

  #TODO: Struct?
  #TODO: Docs

  #hide sharding
  def start_link(token, caller) do
    start_link(token, caller, 1)
  end

  #expose sharding
  def start_link(token, caller, shard_num) do
    gateway = get_gateway()

    :crypto.start
    :ssl.start
    state_map = Map.new([token: token, shard_num: shard_num, caller: caller])
    :websocket_client.start_link(gateway, __MODULE__, state_map)
  end

  def status_update(pid, new_status) do
    send(pid, {:status_update, new_status})
  end

  @doc false
  def websocket_handle({:binary, payload}, state, state_map) do
    case payload do
      _ -> IO.inspect(payload) #state_map.caller.do_something(msg) to run users commands
    end

    {:ok, state_map}
  end

  @doc false
  def send_heartbeat(time) do
    Process.send_after(self, :heartbeat, time)
  end

  @doc false
  def init(state_map) do
    {:once, state_map}
  end

  @doc false
  def onconnect(ws_req, state) do
    #identify :websocket_client.cast(self, {:text, "message"})
    IO.inspect("CONNECTED")
    {:ok, state}
  end

  @doc false
  def ondisconnect(reason, state) do
    {:ok}
  end

  @doc false
  def websocket_info({:status_update, new_status}, ws_req, state) do
    #TODO: Flesh this out
    :websocket_client.cast(self, {new_status})
    {:ok, state}
  end

  @doc false
  def websocket_terminate(close_info, ws_req, state) do
    {:ok}
  end
  
end