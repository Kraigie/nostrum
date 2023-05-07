defmodule Nostrum.Shard.Session do
  @moduledoc false

  alias Nostrum.{Constants, Util}
  alias Nostrum.Shard.{Connector, Event, Payload}
  alias Nostrum.Struct.WSState

  require Logger

  use GenServer

  @gateway_qs "/?compress=zlib-stream&encoding=etf&v=10"

  # Maximum time the initial connection may take, in milliseconds.
  @timeout_connect 10_000
  # Maximum time the websocket upgrade may take, in milliseconds.
  @timeout_ws_upgrade 10_000
  # Messages to buffer at a time. Decremented by :gun by 1 for every message we
  # receive. If this reaches zero, `:gun` will stop reading events from
  # upstream. Equivalent to setting `{active, false}` on the socket at `0`.
  @standard_flow 10

  def update_status(pid, status, game, stream, type) do
    {idle_since, afk} =
      case status do
        "idle" ->
          {Util.now(), true}

        _ ->
          {0, false}
      end

    payload = Payload.status_update_payload(idle_since, game, stream, status, afk, type)
    GenServer.cast(pid, {:status_update, payload})
  end

  def update_voice_state(pid, guild_id, channel_id, self_mute, self_deaf) do
    payload = Payload.update_voice_state_payload(guild_id, channel_id, self_mute, self_deaf)
    GenServer.cast(pid, {:update_voice_state, payload})
  end

  def request_guild_members(pid, guild_id, limit \\ 0) do
    payload = Payload.request_members_payload(guild_id, limit)
    GenServer.cast(pid, {:request_guild_members, payload})
  end

  def get_ws_state(pid) do
    GenServer.call(pid, :get_ws_state)
  end

  def start_link([gateway, shard_num]) do
    GenServer.start_link(__MODULE__, [gateway, shard_num], spawn_opt: [Util.fullsweep_after()])
  end

  def init([_gateway, _shard_num] = args) do
    {:ok, nil, {:continue, args}}
  end

  def handle_continue([gateway, shard_num], nil) do
    Connector.block_until_connect()
    Logger.metadata(shard: shard_num)

    gun_opts = %{protocols: [:http], retry: 1_000_000_000, tls_opts: Constants.gun_tls_opts()}
    {:ok, worker} = :gun.open(:binary.bin_to_list(gateway), 443, gun_opts)
    {:ok, :http} = :gun.await_up(worker, @timeout_connect)
    stream = :gun.ws_upgrade(worker, @gateway_qs, [], %{flow: @standard_flow})
    {:upgrade, ["websocket"], _} = :gun.await(worker, stream, @timeout_ws_upgrade)

    zlib_context = :zlib.open()
    :zlib.inflateInit(zlib_context)

    state = %WSState{
      conn_pid: self(),
      conn: worker,
      shard_num: shard_num,
      stream: stream,
      gateway: gateway <> @gateway_qs,
      last_heartbeat_ack: DateTime.utc_now(),
      heartbeat_ack: true,
      zlib_ctx: zlib_context
    }

    Logger.debug(fn -> "Websocket connection up on worker #{inspect(worker)}" end)

    {:noreply, state}
  end

  def handle_info({:gun_ws, _worker, stream, {:binary, frame}}, state) do
    payload =
      state.zlib_ctx
      |> :zlib.inflate(frame)
      |> :erlang.iolist_to_binary()
      |> :erlang.binary_to_term()

    updated_state = %{state | seq: payload.s || state.seq}

    from_handle =
      payload.op
      |> Constants.atom_from_opcode()
      |> Event.handle(payload, updated_state)

    :ok = :gun.update_flow(updated_state.conn, stream, @standard_flow)

    case from_handle do
      {new_state, reply} ->
        :ok = :gun.ws_send(updated_state.conn, stream, {:binary, reply})
        {:noreply, new_state}

      new_state ->
        {:noreply, new_state}
    end
  end

  def handle_info({:gun_ws, _conn, _stream, :close}, state) do
    Logger.info("Shard websocket closed (unknown reason)")
    {:noreply, state}
  end

  def handle_info({:gun_ws, _conn, _stream, {:close, errno, reason}}, state) do
    Logger.info("Shard websocket closed (errno #{errno}, reason #{inspect(reason)})")
    {:noreply, state}
  end

  def handle_info(
        {:gun_down, _conn, _proto, _reason, _killed_streams},
        state
      ) do
    # Try to cancel the internal timer, but
    # do not explode if it was already cancelled.
    :timer.cancel(state.heartbeat_ref)
    {:noreply, state}
  end

  def handle_info({:gun_up, worker, _proto}, state) do
    :ok = :zlib.inflateReset(state.zlib_ctx)
    stream = :gun.ws_upgrade(worker, @gateway_qs, [], %{flow: @standard_flow})
    {:upgrade, ["websocket"], _} = :gun.await(worker, stream, @timeout_ws_upgrade)
    Logger.warn("Reconnected after connection broke")
    {:noreply, %{state | heartbeat_ack: true}}
  end

  def handle_cast({:status_update, payload}, state) do
    :ok = :gun.ws_send(state.conn, state.stream, {:binary, payload})
    {:noreply, state}
  end

  def handle_cast({:update_voice_state, payload}, state) do
    :ok = :gun.ws_send(state.conn, state.stream, {:binary, payload})
    {:noreply, state}
  end

  def handle_cast({:request_guild_members, payload}, state) do
    :ok = :gun.ws_send(state.conn, state.stream, {:binary, payload})
    {:noreply, state}
  end

  def handle_cast(:heartbeat, %{heartbeat_ack: false, heartbeat_ref: timer_ref} = state) do
    Logger.warn("heartbeat_ack not received in time, disconnecting")
    {:ok, :cancel} = :timer.cancel(timer_ref)
    :gun.ws_send(state.conn, state.stream, :close)
    {:noreply, state}
  end

  def handle_cast(:heartbeat, state) do
    {:ok, ref} =
      :timer.apply_after(state.heartbeat_interval, :gen_server, :cast, [
        state.conn_pid,
        :heartbeat
      ])

    :ok = :gun.ws_send(state.conn, state.stream, {:binary, Payload.heartbeat_payload(state.seq)})

    {:noreply,
     %{state | heartbeat_ref: ref, heartbeat_ack: false, last_heartbeat_send: DateTime.utc_now()}}
  end

  def handle_call(:get_ws_state, _sender, state) do
    {:reply, state, state}
  end
end
