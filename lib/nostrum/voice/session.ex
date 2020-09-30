defmodule Nostrum.Voice.Session do
  @moduledoc false

  alias Nostrum.Constants
  alias Nostrum.Struct.{VoiceState, VoiceWSState}
  alias Nostrum.Util
  alias Nostrum.Voice
  alias Nostrum.Voice.{Event, Payload}

  require Logger

  use GenServer

  @gateway_qs "/?v=4"

  @timeout_connect 10_000

  @timeout_ws_upgrade 10_000

  def start_link(%VoiceState{} = vs) do
    GenServer.start_link(__MODULE__, vs)
  end

  def init(args) do
    {:ok, nil, {:continue, args}}
  end

  def handle_continue(%VoiceState{} = voice, nil) do
    [host, port] = String.split(voice.gateway, ":")
    {:ok, worker} = :gun.open(:binary.bin_to_list(host), String.to_integer(port), %{protocols: [:http]})
    {:ok, :http} = :gun.await_up(worker, @timeout_connect)
    stream = :gun.ws_upgrade(worker, @gateway_qs)
    await_ws_upgrade(worker, stream)

    state = %VoiceWSState{
      conn_pid: self(),
      conn: worker,
      guild: voice.guild,
      session: voice.session,
      token: voice.token,
      gateway: voice.gateway <> @gateway_qs,
      last_heartbeat_ack: DateTime.utc_now(),
      heartbeat_ack: true
    }

    Logger.debug(fn -> "Voice Websocket connection up on worker #{inspect(worker)}" end)
    Voice.update_guild(voice.guild, session_pid: self())
    {:noreply, state}
  end

  defp await_ws_upgrade(worker, stream) do
    receive do
      {:gun_upgrade, ^worker, ^stream, [<<"websocket">>], _headers} ->
        :ok

      {:gun_error, ^worker, ^stream, reason, _headers} ->
        exit({:ws_upgrade_failed, reason})
    after @timeout_ws_upgrade ->
      Logger.error("Voice Websocket upgrade failed after #{@timeout_ws_upgrade / 1000} seconds")
      exit(:timeout)
    end
  end

  def handle_info({:speaking, speaking}, state) do
    voice = Voice.update_guild(state.guild,
      speaking: speaking
    )
    payload = Payload.speaking_payload(voice)
    :ok = :gun.ws_send(state.conn, {:text, payload})
    {:noreply, state}
  end

  def handle_info({:gun_ws, _worker, _stream, {:text, frame}}, state) do
    payload =
      frame
      |> :erlang.iolist_to_binary()
      |> Poison.decode!()
      |> Util.safe_atom_map()

    from_handle =
      payload.op
      |> Constants.atom_from_voice_opcode()
      |> Event.handle(payload, state)

    case from_handle do
      {new_state, reply} ->
        :ok = :gun.ws_send(state.conn, {:text, reply})
        {:noreply, new_state}

      new_state ->
        {:noreply, new_state}
    end

  end

  def handle_info({:gun_ws, _conn, _stream, {:close, errno, reason}}, state) do
    Logger.info("Voice websocket closed (errno #{errno}, reason #{inspect(reason)})")
    Voice.remove_guild(state.guild)
    {:noreply, state}
  end

  def handle_info(
        {:gun_down, _conn, _proto, _reason, _killed_streams, _unprocessed_streams},
        state
      ) do
    # Try to cancel the internal timer, but
    # do not explode if it was already cancelled.
    :timer.cancel(state.heartbeat_ref)
    {:noreply, state}
  end

  def handle_info({:gun_up, worker, _proto}, state) do
    stream = :gun.ws_upgrade(worker, @gateway_qs)
    await_ws_upgrade(worker, stream)
    Logger.warn("Reconnected after connection broke")
    {:noreply, %{state | heartbeat_ack: true}}
  end

  def handle_cast(:heartbeat, %{heartbeat_ack: false, heartbeat_ref: timer_ref} = state) do
    Logger.warn("heartbeat_ack not received in time, disconnecting")
    {:ok, :cancel} = :timer.cancel(timer_ref)
    :gun.ws_send(state.conn, :close)
    {:noreply, state}
  end

  def handle_cast(:heartbeat, state) do
    {:ok, ref} =
      :timer.apply_after(state.heartbeat_interval |> trunc, :gen_server, :cast, [
        state.conn_pid,
        :heartbeat
      ])

    :ok = :gun.ws_send(state.conn, {:text, Payload.heartbeat_payload()})

    {:noreply,
     %{state | heartbeat_ref: ref, heartbeat_ack: false, last_heartbeat_send: DateTime.utc_now()}}
  end
end
