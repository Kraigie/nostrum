defmodule Nostrum.Voice.Session do
  @moduledoc false

  alias Nostrum.Cache.{ChannelCache, GuildCache}
  alias Nostrum.Constants
  alias Nostrum.ConsumerGroup
  alias Nostrum.Shard.Dispatch
  alias Nostrum.Struct.{VoiceState, VoiceWSState}
  alias Nostrum.Voice
  alias Nostrum.Voice.{Event, Opus, Payload}

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
    Logger.metadata(
      guild: ~s|"#{GuildCache.get!(voice.guild_id).name}"|,
      channel: ~s|"#{ChannelCache.get!(voice.channel_id).name}"|
    )

    [host, port] = String.split(voice.gateway, ":")

    gun_opts = %{protocols: [:http], retry: 1_000_000_000, tls_opts: Constants.gun_tls_opts()}
    {:ok, worker} = :gun.open(:binary.bin_to_list(host), String.to_integer(port), gun_opts)

    {:ok, :http} = :gun.await_up(worker, @timeout_connect)
    stream = :gun.ws_upgrade(worker, @gateway_qs)
    {:upgrade, ["websocket"], _} = :gun.await(worker, stream, @timeout_ws_upgrade)

    state = %VoiceWSState{
      conn_pid: self(),
      conn: worker,
      guild_id: voice.guild_id,
      channel_id: voice.channel_id,
      ssrc_map: Map.new(),
      session: voice.session,
      token: voice.token,
      gateway: voice.gateway,
      stream: stream,
      last_heartbeat_ack: DateTime.utc_now(),
      heartbeat_ack: true
    }

    Logger.debug(fn -> "Voice Websocket connection up on worker #{inspect(worker)}" end)
    Voice.update_voice(voice.guild_id, session_pid: self())
    {:noreply, state}
  end

  def get_ws_state(pid) do
    GenServer.call(pid, :ws_state)
  end

  def close_connection(pid) do
    GenServer.cast(pid, :close)
  end

  def set_speaking(pid, speaking, timed_out \\ false) do
    GenServer.cast(pid, {:speaking, speaking, timed_out})
  end

  def on_voice_ready(pid) do
    GenServer.cast(pid, :voice_ready)
  end

  def handle_info({:gun_ws, _worker, stream, {:text, frame}}, state) do
    # Jason.decode calls iodata_to_binary internally
    payload = Jason.decode!(frame)

    from_handle =
      payload["op"]
      |> Constants.atom_from_voice_opcode()
      |> Event.handle(payload, state)

    case from_handle do
      {new_state, reply} ->
        :ok = :gun.ws_send(state.conn, stream, {:text, reply})
        {:noreply, new_state}

      new_state ->
        {:noreply, new_state}
    end
  end

  def handle_info({:gun_ws, _conn, _stream, {:close, errno, reason}}, state) do
    Logger.info("Voice websocket closed (errno #{errno}, reason #{inspect(reason)})")

    # If we received an errno of 4006, session is no longer valid, so we must get a new session.
    if errno == 4006 do
      {:stop, {:shutdown, :restart}, state}
    else
      {:noreply, state}
    end
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
    stream = :gun.ws_upgrade(worker, @gateway_qs)
    {:upgrade, ["websocket"], _} = :gun.await(worker, stream, @timeout_ws_upgrade)
    Logger.warn("Reconnected after connection broke")
    {:noreply, %{state | heartbeat_ack: true}}
  end

  def handle_info({:udp, _erl_port, _ip, _port, packet}, state) do
    case packet do
      # Skip RTCP packets
      <<2::2, 0::1, 1::5, 201::8, _rest::binary>> ->
        :noop

      <<header::binary-size(12), data::binary>> ->
        nonce = header <> <<0::8*12>>
        payload = Kcl.secretunbox(data, nonce, state.secret_key)
        <<_::16, seq::integer-16, time::integer-32, ssrc::integer-32>> = header
        opus = Opus.strip_rtp_ext(payload)
        incoming_packet = Payload.voice_incoming_packet({{seq, time, ssrc}, opus})

        {incoming_packet, state}
        |> Dispatch.handle()
        |> ConsumerGroup.dispatch()
    end

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
      :timer.apply_after(state.heartbeat_interval |> trunc, :gen_server, :cast, [
        state.conn_pid,
        :heartbeat
      ])

    :ok = :gun.ws_send(state.conn, state.stream, {:text, Payload.heartbeat_payload()})

    {:noreply,
     %{state | heartbeat_ref: ref, heartbeat_ack: false, last_heartbeat_send: DateTime.utc_now()}}
  end

  def handle_cast(:voice_ready, state) do
    voice = Voice.get_voice(state.guild_id)
    voice_ready = Payload.voice_ready_payload(voice)

    {voice_ready, state}
    |> Dispatch.handle()
    |> ConsumerGroup.dispatch()

    {:noreply, state}
  end

  def handle_cast({:speaking, speaking, timed_out}, state) do
    voice = Voice.update_voice(state.guild_id, speaking: speaking)
    speaking_update = Payload.speaking_update_payload(voice, timed_out)
    payload = Payload.speaking_payload(voice)

    {speaking_update, state}
    |> Dispatch.handle()
    |> ConsumerGroup.dispatch()

    :ok = :gun.ws_send(state.conn, state.stream, {:text, payload})
    {:noreply, state}
  end

  def handle_cast(:close, state) do
    {:stop, {:shutdown, :close}, state}
  end

  def handle_call(:ws_state, _from, state) do
    {:reply, state, state}
  end

  def terminate({:shutdown, :restart}, state) do
    :gun.close(state.conn)
    restart_session_async(state)
  end

  def terminate({:shutdown, :close}, state) do
    :gun.close(state.conn)
  end

  def restart_session_async(state) do
    spawn(fn ->
      Process.monitor(state.conn_pid)

      %VoiceState{channel_id: chan, self_mute: mute, self_deaf: deaf, persist_source: persist} =
        Voice.get_voice(state.guild_id)

      receive do
        _ ->
          Voice.leave_channel(state.guild_id)
          Voice.join_channel(state.guild_id, chan, mute, deaf, persist)
      end
    end)
  end
end
