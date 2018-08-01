defmodule Nostrum.Shard.Session do
  @moduledoc false

  use WebSockex

  alias Nostrum.{Constants, Util}
  alias Nostrum.Shard.{Connector, Event, Payload}
  alias Nostrum.Struct.WSState

  require Logger

  @gateway_qs "/?compress=zlib-stream&encoding=etf&v=6"

  def update_status(pid, status, game, stream, type) do
    {idle_since, afk} =
      case status do
        "idle" ->
          {Util.now(), true}

        _ ->
          {0, false}
      end

    payload = Payload.status_update_payload(idle_since, game, stream, status, afk, type)
    WebSockex.cast(pid, {:status_update, payload})
  end

  def request_guild_members(pid, guild_id, limit \\ 0) do
    payload = Payload.request_members_payload(guild_id, limit)
    WebSockex.cast(pid, {:request_guild_members, payload})
  end

  def start_link([gateway, shard_num]) do
    state = %WSState{
      shard_num: shard_num,
      gateway: gateway <> @gateway_qs,
      last_heartbeat_ack: DateTime.utc_now(),
      heartbeat_ack: true
    }

    Connector.block_until_connect()
    # TODO: Add support for `spawn_opt` start arguments to WebSockex, this does nothing until then.
    WebSockex.start_link(gateway <> @gateway_qs, __MODULE__, state, spawn_opt: [Util.fullsweep_after()])
  end

  def handle_connect(conn, state) do
    Logger.metadata(shard: state.shard_num)

    zlib_ctx = :zlib.open()
    :zlib.inflateInit(zlib_ctx)

    {:ok,
     %{
       state
       | conn: conn,
         conn_pid: self(),
         zlib_ctx: zlib_ctx,
         heartbeat_ack: true
     }}
  end

  def handle_frame({:binary, frame}, state) do
    payload =
      state.zlib_ctx
      |> :zlib.inflate(frame)
      |> :erlang.iolist_to_binary()
      |> :erlang.binary_to_term()

    state = %{state | seq: payload.s || state.seq}

    from_handle =
      payload.op
      |> Constants.atom_from_opcode()
      |> Event.handle(payload, state)

    case from_handle do
      {new_state, reply} ->
        {:reply, {:binary, reply}, new_state}

      new_state ->
        {:ok, new_state}
    end
  end

  def handle_cast({:status_update, payload}, state) do
    {:reply, {:binary, payload}, state}
  end

  def handle_cast({:request_guild_members, payload}, state) do
    {:reply, {:binary, payload}, state}
  end

  def handle_cast(:heartbeat, %{heartbeat_ack: false} = state) do
    Logger.warn("heartbeat_ack not received in time, disconnecting")
    {:close, state}
  end

  def handle_cast(:heartbeat, state) do
    {:ok, ref} =
      :timer.apply_after(state.heartbeat_interval, WebSockex, :cast, [state.conn_pid, :heartbeat])

    {:reply, {:binary, Payload.heartbeat_payload(state.seq)},
     %{state | heartbeat_ref: ref, heartbeat_ack: false, last_heartbeat_send: DateTime.utc_now()}}
  end

  def handle_disconnect(%{reason: reason}, state) when is_tuple(reason) do
    Logger.warn(fn ->
      "websocket disconnected with reason #{inspect(reason)}, attempting reconnect"
    end)

    :timer.cancel(state.heartbeat_ref)

    {:reconnect, state}
  end

  def handle_disconnect(%{reason: reason}, state) do
    Logger.warn(fn ->
      "websocket errored with reason #{inspect(reason)}, attempting reconnect"
    end)

    :timer.cancel(state.heartbeat_ref)

    {:reconnect, state}
  end

  def terminate(reason, state) do
    :timer.cancel(state.heartbeat_ref)

    Logger.warn(fn ->
      "websocket closed with reason #{inspect(reason)}"
    end)
  end
end
