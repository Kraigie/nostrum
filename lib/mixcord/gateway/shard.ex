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
    :crypto.start
    :ssl.start
    state_map = Map.new([token: token, shard_num: shard_num, caller: caller])
    :websocket_client.start_link(gateway() , __MODULE__, state_map)
  end

  def status_update(pid, new_status) do
    send(pid, {:status_update, new_status})
  end

  @doc false
  def websocket_handle({:binary, payload}, _state, state_map) do
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
  def onconnect(_ws_req, state_map) do
    #identify :websocket_client.cast(self, {:text, "message"})
    IO.inspect("CONNECTED")
    {:ok, state_map}
  end

  @doc false
  def ondisconnect(_reason, _state) do
    {:ok}
  end

  @doc false
  def websocket_info({:status_update, new_status}, _ws_req, state) do
    #TODO: Flesh this out
    :websocket_client.cast(self, {new_status})
    {:ok, state}
  end

  @doc false
  def websocket_terminate(_close_info, _ws_req, _state) do
    {:ok}
  end

  @doc false
  def gateway do
    if Enum.member?(:ets.all, :gateway_url) do
      url = :ets.lookup(:gateway_url, "url")
        |> List.first
      url["url"]
    else
      :ets.new(:gateway_url, [:named_table])
      new_url = get_new_gateway_url()
      :ets.insert(:gateway_url, {"url", new_url})
      new_url
    end
  end

  defp get_new_gateway_url do
    case Client.request(:get, Constants.gateway, "") do
      {:error, status_code: status_code, message: message} ->
        raise(Mixcord.Errors.ApiError, status_code: status_code, message: message)
      {:ok, body: body} ->
        body = Poison.decode!(body)
        gateway_url = body["url"]
    end
  end

end