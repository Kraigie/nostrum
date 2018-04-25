defmodule Nostrum.Shard.Session do
  @moduledoc false

  use GenServer

  alias Nostrum.{Constants, Util}
  alias Nostrum.Shard.{Connector, Event, Heartbeat, Payload}

  require Logger

  @gateway_qs if Application.get_env(:nostrum, :zlib_stream, false),
                do: '/?compress=zlib-stream&encoding=etf&v=6',
                else: '/?encoding=etf&v=6'

  def update_status(pid, status, game, stream, type) do
    {idle_since, afk} =
      case status do
        "idle" ->
          {Util.now(), true}

        _ ->
          {0, false}
      end

    payload = Payload.status_update_payload(idle_since, game, stream, status, afk, type)
    send(pid, {:status_update, payload})
  end

  def request_guild_members(pid, guild_id, limit \\ 0) do
    payload = Payload.request_members_payload(guild_id, limit)
    send(pid, {:request_guild_members, payload})
  end

  def start_link([args]) do
    GenServer.start_link(__MODULE__, args)
  end

  def init(%{gw: g, shard_num: s, token: t}) do
    Logger.metadata(shard: s)

    {:ok, heartbeat_pid} = Heartbeat.start_link()

    erl_gw =
      ~r/wss:\/\//
      |> Regex.replace(g, "")
      |> to_charlist

    zlib_ctx =
      if use_zlib_stream?() do
        zlib_ctx = :zlib.open()
        :zlib.inflateInit(zlib_ctx)
        zlib_ctx
      end

    case await_connect(erl_gw) do
      {:error, reason} ->
        {:stop, reason}

      {:ok, conn} ->
        Heartbeat.update_gun_onwer(heartbeat_pid, self())

        if use_zlib_stream?() do
          state = %{
            token: t,
            shard_num: s,
            seq: nil,
            session: nil,
            shard_pid: self(),
            heartbeat_pid: heartbeat_pid,
            gun_pid: conn,
            gateway: erl_gw,
            last_heartbeat_send: nil,
            last_heartbeat_ack: nil,
            zlib_ctx: zlib_ctx,
            zlib_buffer: <<>>
          }

          {:ok, state}
        else
          state = %{
            token: t,
            shard_num: s,
            seq: nil,
            session: nil,
            shard_pid: self(),
            heartbeat_pid: heartbeat_pid,
            gun_pid: conn,
            gateway: erl_gw,
            last_heartbeat_send: nil,
            last_heartbeat_ack: nil
          }

          {:ok, state}
        end
    end
  end

  def await_connect(erl_gw) do
    Connector.block_until_connect()

    with {:ok, conn} <- :gun.open(erl_gw, 443, %{protocols: [:http], retry: 5, retry_timeout: 0}),
         Process.monitor(conn),
         {:ok, :http} <- :gun.await_up(conn) do
      :gun.ws_upgrade(conn, @gateway_qs)
      {:ok, conn}
    end
  end

  def handle_info({:gun_ws_upgrade, _conn, :ok, _headers}, state) do
    Heartbeat.ack(state.heartbeat_pid)
    {:noreply, state}
  end

  def handle_info({:gun_ws, conn, {:binary, frame}}, %{zlib_ctx: _} = state) do
    buffer = state.zlib_buffer <> frame
    buffer_head_size = byte_size(buffer) - 4

    case buffer do
      <<_::bytes-size(buffer_head_size), 0, 0, 0xFF, 0xFF>> ->
        payload =
          state.zlib_ctx
          |> :zlib.inflate(buffer)
          |> :erlang.iolist_to_binary()
          |> :erlang.binary_to_term()

        new_state =
          payload.op
          |> Constants.atom_from_opcode()
          |> Event.handle(payload, conn, state)

        seq = payload.s || state.seq
        Heartbeat.update_sequence(state.heartbeat_pid, seq)

        {:noreply, %{new_state | seq: seq, zlib_buffer: <<>>}}

      _ ->
        {:noreply, %{state | zlib_buffer: buffer}}
    end
  end

  def handle_info({:gun_ws, conn, {:binary, frame}}, state) do
    payload = :erlang.binary_to_term(frame)

    new_state =
      payload.op
      |> Constants.atom_from_opcode()
      |> Event.handle(payload, conn, state)

    {:noreply, %{new_state | seq: payload.s || state.seq}}
  end

  def handle_info({:gun_ws, _conn, {:close, code, message}}, %{zlib_ctx: _} = state) do
    Logger.warn("websocket closing: #{code} #{message}")
    {:noreply, %{state | zlib_buffer: <<>>}}
  end

  def handle_info({:gun_ws, _conn, {:close, code, message}}, state) do
    Logger.warn("websocket closing: #{code} #{message}")
    {:noreply, state}
  end

  # Gun reopeneded a connection, block and attempt upgrade
  def handle_info({:gun_up, conn, :http}, state) do
    Connector.block_until_connect()
    :gun.ws_upgrade(conn, @gateway_qs)
    {:noreply, %{state | gun_pid: conn}}
  end

  def handle_info(
        {:gun_down, _conn, :ws, :closed, _maybe_processed, _open_streams},
        %{zlib_ctx: _} = state
      ) do
    Logger.info("websocket closed, attempting reconnect")
    :zlib.inflateReset(state.zlib_ctx)
    {:noreply, %{state | zlib_buffer: <<>>}}
  end

  def handle_info({:gun_down, _conn, :ws, :closed, _maybe_processed, _open_streams}, state) do
    Logger.info("websocket closed, attempting reconnect")
    {:noreply, state}
  end

  def handle_info({:DOWN, _m_ref, :process, _conn, reason}, state) do
    Logger.warn(fn ->
      "websocket caller received DOWN with reason #{inspect(reason)}, attempting reconnect"
    end)

    Process.exit(state.heartbeat_pid, :kill)
    :zlib.inflateReset(state.zlib_ctx)
    {:ok, conn} = await_connect(state.gateway)
    {:noreply, %{state | gun_pid: conn}}
  end

  def handle_info({:status_update, status}, state) do
    Event.gun_send(state.gun_pid, status)
    {:noreply, state}
  end

  def handle_info({:request_guild_members, req}, state) do
    Event.gun_send(state.gun_pid, req)
    {:noreply, state}
  end

  def handle_info({:send_heartbeat, seq}, state) do
    Event.gun_send(state.gun_pid, Payload.heartbeat_payload(seq))
    {:noreply, %{state | last_heartbeat_send: DateTime.utc_now()}}
  end

  def handle_info(:force_disconnect, state) do
    :gun.shutdown(state.gun_pid)
    {:noreply, state}
  end

  def handle_info(m, state) do
    Logger.warn("Unhandled websocket message: #{inspect(m)}")
    {:noreply, state}
  end

  def use_zlib_stream? do
    Application.get_env(:nostrum, :zlib_stream, false)
  end
end
