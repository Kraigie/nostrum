defmodule Mixcord.Shard do
  @moduledoc false

  @behaviour :websocket_client

  alias Mixcord.Shard.{Event, Payload}
  alias Mixcord.{Constants, Util}
  require Logger

  @connect_wait 5000

  def start_link(token, caller, shard_num) do
    :crypto.start
    :ssl.start
    # This makes the supervisor spawn a shard worker ever 5 seconds. This only occurs on ShardSupervisor start.
    # If an individual shard fails, it will be restarted immediately.
    # TODO: Queue reconnects/check this better
    if Util.num_shards > 1, do: Process.sleep(@connect_wait)
    :websocket_client.start_link(Util.gateway, __MODULE__, Payload.state_map(token, caller, shard_num, self()))
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
    now = Util.now()
    :websocket_client.cast(self(), {:binary, Payload.heartbeat_payload(state.seq)})
    Event.heartbeat(self(), interval)
    {:ok, %{state | last_heartbeat: now}}
  end

  def websocket_info(:identify, _ws_req, state) do
    :websocket_client.cast(self(), {:binary, Payload.identity_payload(state)})
    {:ok, state}
  end

  def websocket_info(:resume, _ws_req, state) do
    :websocket_client.cast(self(), {:binary, Payload.resume_payload(state)})
    {:ok, state}
  end

  def init(state) do
    {:once, state}
  end

  def onconnect(_ws_req, state) do
    Logger.debug "SHARD #{state.shard_num} CONNECTED"
    {:ok, state}
  end

  def ondisconnect(reason, state) do
    Logger.warn "WS DISCONNECTED BECAUSE: #{inspect reason}"
    Logger.debug "STATE ON CLOSE: #{inspect state}"
    if state.reconnect_attempts > 3 do
      {:close, reason, state}
    else
      :timer.sleep(5000 * round :math.pow(1.5, state.reconnect_attempts))
      Logger.debug "RECONNECT ATTEMPT NUMBER #{state.reconnect_attempts + 1}"
      {:reconnect, %{state | reconnect_attempts: state.reconnect_attempts + 1}}
    end
  end

  def websocket_terminate(reason, _ws_req, _state) do
    Logger.debug "WS TERMINATED BECAUSE: #{inspect reason}"
    :ok
  end

end
