defmodule Nostrum.Voice.Event do
  @moduledoc false

  alias Nostrum.Cache.UserCache
  alias Nostrum.Voice
  alias Nostrum.Voice.{Audio, Payload, Session}

  require Logger

  def handle(:ready, payload, state) do
    Logger.debug("VOICE READY")

    voice =
      Voice.update_voice(state.guild_id,
        ssrc: payload["d"]["ssrc"],
        ip: payload["d"]["ip"],
        port: payload["d"]["port"],
        udp_socket: Audio.open_udp()
      )

    {my_ip, my_port} = Audio.discover_ip(voice.udp_socket, voice.ip, voice.port, voice.ssrc)

    {state, Payload.select_protocol_payload(my_ip, my_port)}
  end

  def handle(:session_description, payload, state) do
    Logger.debug("VOICE SESSION DESCRIPTION")

    secret_key = payload["d"]["secret_key"] |> :erlang.list_to_binary()

    Voice.update_voice(state.guild_id,
      secret_key: secret_key,
      rtp_sequence: 0,
      rtp_timestamp: 0
    )

    Session.on_voice_ready(state.conn_pid)

    %{state | secret_key: secret_key}
  end

  def handle(:heartbeat_ack, _payload, state) do
    Logger.debug("VOICE HEARTBEAT_ACK")
    %{state | last_heartbeat_ack: DateTime.utc_now(), heartbeat_ack: true}
  end

  def handle(:resumed, _payload, state) do
    Logger.info("VOICE RESUMED")
    state
  end

  def handle(:hello, payload, state) do
    state = %{state | heartbeat_interval: payload["d"]["heartbeat_interval"]}

    GenServer.cast(state.conn_pid, :heartbeat)

    if state.identified do
      Logger.info("RESUMING")
      {state, Payload.resume_payload(state)}
    else
      Logger.info("IDENTIFYING")
      {%{state | identified: true}, Payload.identify_payload(state)}
    end
  end

  def handle(:client_connect, payload, state) do
    Logger.debug(fn ->
      user_id = payload["d"]["user_id"] |> String.to_integer()

      "Voice client connected: #{case UserCache.get(user_id) do
        {:ok, %{username: username}} -> username
        _ -> user_id
      end}"
    end)

    state
  end

  def handle(:client_disconnect, payload, state) do
    Logger.debug(fn ->
      user_id = payload["d"]["user_id"] |> String.to_integer()

      "Voice client disconnected: #{case UserCache.get(user_id) do
        {:ok, %{username: username}} -> username
        _ -> user_id
      end}"
    end)

    state
  end

  def handle(:codec_info, _payload, state), do: state

  def handle(:speaking, payload, state) do
    ssrc = payload["d"]["ssrc"]
    user_id = payload["d"]["user_id"] |> String.to_integer()
    ssrc_map = Map.put(state.ssrc_map, ssrc, user_id)
    %{state | ssrc_map: ssrc_map}
  end

  def handle(event, _payload, state) do
    Logger.warn("UNHANDLED VOICE GATEWAY EVENT #{event}")
    state
  end
end
