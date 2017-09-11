defmodule Nostrum.Shard.Session do
  @moduledoc false

  use GenServer

  alias Nostrum.{Constants, Util}
  alias Nostrum.Shard.{Connector, Event, Payload}
  alias Nostrum.Shard.Stage.{Cache, Producer}

  require Logger

  @typedoc """
  The state map maintained by each websocket process.

  Keys
   * `token` - The token of the bot.
   * `shard_num` - The shard number container this state.
   * `seq` - Current seq number of the websocket.
   * `session` - Current session of the websocket.
   * `reconnect_attempts` - Current number of reconnect attempts.
   * `last_heartbeat` - The time of the last heartbeat.
   * `shard_pid` - Pid of the shard containing this state.
   * `producer_pid` - Pid of the producer attached to this shard.
   * `heartbeat_ack` - Whether we received a heartbeack_ack or not (initialized to true).
   * `gun_pid` - Pid of the WS connection process.
  """
  @type state_map :: map

  @gateway_qs '/?encoding=etf&v=6'

  def update_status(pid, status, game, stream) do
    {idle_since, afk} =
      case status do
        "idle" ->
          {Util.now(), true}
        _ ->
          {0, false}
      end
    payload = Payload.status_update_payload(idle_since, game, status, afk, stream)
    send(pid, {:status_update, payload})
  end


  def request_guild_members(pid, guild_id, limit \\ 0) do
    payload = Payload.request_members_payload(guild_id, limit)
    send(pid, {:request_guild_members, payload})
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  def init(%{gw: g, shard_num: s, token: t}) do
    {:ok, producer_pid} = Producer.start_link()
    Cache.start_link(producer_pid)

    erl_gw =
      ~r/wss:\/\//
      |> Regex.replace(g, "")
      |> to_charlist

    case await_connect(erl_gw) do
      {:error, reason} ->
        {:stop, reason}
      {:ok, conn} ->
        {:ok, state_map(t, s, self(), producer_pid, conn, erl_gw)}
    end
  end

  def await_connect(erl_gw) do
    Connector.block_until_connect()
    with {:ok, conn} <- :gun.open(erl_gw, 443, %{protocols: [:http], retry: 5, retry_timeout: 0}),
         Process.monitor(conn),
         {:ok, :http} <- :gun.await_up(conn)
    do
      :gun.ws_upgrade(conn, @gateway_qs)
      {:ok, conn}
    end
  end

  def handle_info({:gun_ws_upgrade, _conn, :ok, _headers}, state) do
    {:noreply, %{state | heartbeat_ack: true}}
  end

  def handle_info({:gun_ws, conn, {:binary, frame}}, state) do
    payload = :erlang.binary_to_term(frame)
    new_state =
      Constants.atom_from_opcode(payload.op)
      |> Event.handle(payload, conn, state)

    {:noreply, %{new_state | seq: payload.s || state.seq}}
  end

  def handle_info({:gun_ws, _conn, {:close, code, message}}, state) do
    Logger.warn "websocket closing: #{code} #{message}"
    {:noreply, state}
  end

  # Gun reopeneded a connection, block and attempt upgrade
  def handle_info({:gun_up, conn, :http}, state) do
    Connector.block_until_connect()
    :gun.ws_upgrade(conn, @gateway_qs)
    {:noreply, %{state | gun_pid: conn}}
  end

  def handle_info({:gun_down, _conn, :ws, :closed, _maybe_processed, _open_streams}, state) do
    Logger.debug "websocket closed, attempting reconnect"
    {:noreply, state}
  end

  def handle_info({:DOWN, _m_ref, :process, _conn, :normal}, state) do
    Logger.warn "websocket caller received DOWN, attempting reconnect"
    {:ok, conn} = await_connect(state.gateway)
    {:noreply, %{state | gun_pid: conn}}
  end

  def handle_info({:heartbeat, interval, conn}, %{heartbeat_ack: true} = state) do
    Event.gun_send(conn, Payload.heartbeat_payload(state.seq))
    Event.heartbeat(conn, interval)
    {:noreply, %{state | heartbeat_ack: false}}
  end

  def handle_info({:heartbeat, _interval, conn}, state) do
    Logger.warn "HEARTBEAT_ACK not received in time, disconnecting"
    :gun.shutdown(conn)
    {:noreply, state}
  end

  def handle_info({:status_update, status}, state) do
    Event.gun_send(state.gun_pid, status)
    {:noreply, state}
  end

  def handle_info({:request_guild_members, req}, state) do
    Event.gun_send(state.gun_pid, req)
    {:noreply, state}
  end

  def handle_info(m, state) do
    Logger.warn "Unhandled websocket message: #{inspect m}"
    {:noreply, state}
  end

  def state_map(token, shard_num, shard_pid, producer_pid, gun_pid, gw) do
    %{
      token: token,
      shard_num: shard_num,
      seq: nil,
      session: nil,
      reconnect_attempts: 0,
      shard_pid: shard_pid,
      producer_pid: producer_pid,
      heartbeat_ack: true,
      gun_pid: gun_pid,
      gateway: gw
    }
  end
end
