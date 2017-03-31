defmodule Nostrum.Shard do
  @moduledoc false

  @behaviour :websocket_client

  alias Nostrum.Shard.{Connector, Event, Payload}
  alias Nostrum.Shard.Dispatch.ProducerSupervisor
  alias Nostrum.{Constants, Util}

  require Logger

  def start_link(gateway, token, shard_num) do
    Connector.block_until_connect()
    :websocket_client.start_link(gateway, __MODULE__, [t: token, s: shard_num])
  end

  def init(t: token, s: shard_num) do
    with \
      {:ok, supervisor} <- ProducerSupervisor.start_link(),
      {:ok, child_pid} <- ProducerSupervisor.start_producer(supervisor, shard_num)
    do
      :ets.insert(:shard_pid_num, {shard_num, self()})
      state = Payload.state_map(token, shard_num, self())
      {:once, %{state | producer_pid: child_pid}}
    end
  end

  def onconnect(_ws_req, state) do
    Logger.debug "SHARD #{state.shard_num} CONNECTED"
    {:ok, state}
  end

  def websocket_handle({:binary, payload}, _state, state) do
    payload = :erlang.binary_to_term(payload)
    new_state = Constants.atom_from_opcode(payload.op)
      |> Event.handle(payload, state)
    {:ok, %{new_state | seq: payload.s || state.seq}}
  end

  def update_status(pid, status, game) do
    {idle_since, afk} =
      case status do
        "idle" ->
          {Util.now(), true}
        _ ->
          {0, false}
      end
    payload = Payload.status_update_payload(idle_since, game, status, afk)
    send(pid, {:status_update, payload})
  end

  def request_guild_members(pid, guild_id, limit \\ 0) do
    payload = Payload.request_members_payload(guild_id, limit)
    send(pid, {:request_guild_members, payload});
  end

  def websocket_info({:request_guild_members, payload}, _ws_req, state) do
    :websocket_client.cast(self(), {:binary, payload})
    {:ok, state}
  end

  def websocket_info({:status_update, payload}, _ws_req, state) do
    :websocket_client.cast(self(), {:binary, payload})
    {:ok, state}
  end

  def websocket_info({:heartbeat, interval}, _ws_req, state) do
    if state.heartbeat_ack do
      :websocket_client.cast(self(), {:binary, Payload.heartbeat_payload(state.seq)})
      Event.heartbeat(self(), interval)
      {:ok, %{state | heartbeat_ack: false}}
    else
      Logger.warn "HEARTBEAT_ACK not received in time. Disconnecting..."
      {:close, "", state}
    end
  end

  def websocket_info(:identify, _ws_req, state) do
    :websocket_client.cast(self(), {:binary, Payload.identity_payload(state)})
    {:ok, state}
  end

  def websocket_info(:resume, _ws_req, state) do
    :websocket_client.cast(self(), {:binary, Payload.resume_payload(state)})
    {:ok, state}
  end

  def ondisconnect(reason, state) do
    Logger.warn "WS DISCONNECTED BECAUSE: #{inspect reason}"
    Logger.debug "STATE ON CLOSE: #{inspect state}"

    if state.reconnect_attempts > 3 do
      {:close, reason, state}
    else
      Process.sleep(5000 * round :math.pow(1.5, state.reconnect_attempts))
      Logger.debug "RECONNECT ATTEMPT NUMBER #{state.reconnect_attempts + 1}"
      {:reconnect, %{state | reconnect_attempts: state.reconnect_attempts + 1}}
    end
  end

  def websocket_terminate(reason, _ws_req, _state) do
    Logger.debug "WS TERMINATED BECAUSE: #{inspect reason}"
    :ok
  end

end
