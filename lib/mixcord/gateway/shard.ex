defmodule Mixcord.Gateway do
  @behaviour :websocket_client
  @moduledoc false


  #get websocket calls
  #@behaviour

  #start link, notify and call things, user will implement receivinmg the genevents
  def start_link do
    :crypto.start
    :ssl.start
    IO.puts "STARTED"
    :websocket_client.start_link('wss://echo.websocket.org', __MODULE__, [])
  end

  def init(conn_state) do
    IO.puts "INITED"
    {:once, 2}
  end

  def onconnect(arg0, state) do
    IO.puts "CONNECTED"
    :websocket_client.cast(self, {:text, "message"})
    {:ok, state}
  end

  def ondisconnect(reason, state) do
    {:ok}
  end

  def websocket_info(any, arg1, state) do
    IO.puts "INFO"
    {:ok}
  end

  def websocket_handle(something, conn_state, state) do
    IO.inspect(something)
    {:ok, state}
  end

  def websocket_terminate(arg0, arg1, state) do
    {:ok}
  end

  def send_heartbeat(time) do
    Process.send_after(self, :heartbeat, time)
  end
end