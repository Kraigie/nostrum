defmodule Nostrum.Voice.Event do
  @moduledoc false

  alias Nostrum.Cache.Me
  alias Nostrum.Constants
  alias Nostrum.Struct.VoiceWSState
  alias Nostrum.Voice
  alias Nostrum.Voice.Audio
  alias Nostrum.Voice.Crypto
  alias Nostrum.Voice.Payload
  alias Nostrum.Voice.Session

  require Logger

  @spec handle(map(), VoiceWSState.t()) :: VoiceWSState.t() | {VoiceWSState.t(), iodata()}
  def handle(payload, state) do
    state = update_sequence(state, payload)

    payload["op"]
    |> Constants.atom_from_voice_opcode()
    |> handle_event(payload["d"], state)
  end

  defp update_sequence(state, %{"seq" => seq} = _payload), do: %{state | seq: seq}

  defp update_sequence(state, _payload), do: state

  defp handle_event(:ready, data, state) do
    Logger.debug("VOICE READY")

    mode = Crypto.encryption_mode(state.bot_options, data["modes"])

    voice =
      Voice.update_voice(state.voice_pid, state.guild_id,
        ssrc: data["ssrc"],
        ip: data["ip"],
        port: data["port"],
        encryption_mode: mode,
        udp_socket: Audio.open_udp()
      )

    {my_ip, my_port} = Audio.discover_ip(voice.udp_socket, voice.ip, voice.port, voice.ssrc)

    {%{state | encryption_mode: mode}, Payload.select_protocol(my_ip, my_port, mode)}
  end

  defp handle_event(:session_description, data, state) do
    Logger.debug("VOICE SESSION DESCRIPTION")

    secret_key = data["secret_key"] |> :erlang.list_to_binary()

    Voice.update_voice_async(state.voice_pid, state.guild_id,
      secret_key: secret_key,
      rtp_sequence: 0,
      rtp_timestamp: 0
    )

    Session.on_voice_ready(state.conn_pid)

    state = %{state | secret_key: secret_key}

    init_dave(data["dave_protocol_version"] || 0, state)
  end

  defp handle_event(:heartbeat_ack, _payload, state) do
    Logger.debug("VOICE HEARTBEAT_ACK")
    %{state | last_heartbeat_ack: DateTime.utc_now(), heartbeat_ack: true}
  end

  defp handle_event(:resumed, _payload, state) do
    Logger.info("VOICE RESUMED")
    state
  end

  defp handle_event(:hello, data, state) do
    state = %{state | heartbeat_interval: data["heartbeat_interval"]}

    GenServer.cast(state.conn_pid, :heartbeat)

    if state.identified do
      Logger.info("RESUMING")
      {state, Payload.resume(state)}
    else
      Logger.info("IDENTIFYING")
      {%{state | identified: true}, Payload.identify(state)}
    end
  end

  defp handle_event(:client_connect, data, %{connected_clients: clients} = state) do
    user_id = data["user_id"] |> String.to_integer()

    Logger.debug("Voice client connected: #{user_id}")

    %{state | connected_clients: MapSet.put(clients, user_id)}
  end

  defp handle_event(:clients_connect, data, %{connected_clients: clients} = state) do
    user_ids = data["user_ids"] |> Enum.map(&String.to_integer/1)

    Logger.debug("Voice clients connected: #{inspect(user_ids)}")

    %{state | connected_clients: MapSet.union(clients, MapSet.new(user_ids))}
  end

  defp handle_event(:client_disconnect, data, %{connected_clients: clients} = state) do
    user_id = data["user_id"] |> String.to_integer()

    Logger.debug("Voice client disconnected: #{user_id}")

    %{state | connected_clients: MapSet.delete(clients, user_id)}
  end

  defp handle_event(:codec_info, _payload, state), do: state

  defp handle_event(:speaking, data, state) do
    ssrc = data["ssrc"]
    user_id = data["user_id"] |> String.to_integer()
    ssrc_map = Map.put(state.ssrc_map, ssrc, user_id)
    %{state | ssrc_map: ssrc_map}
  end

  ## DAVE specific events

  defp handle_event(:dave_prepare_transition, data, state) do
    protocol_version = data["protocol_version"]
    transition_id = data["transition_id"]

    state = %{
      state
      | pending_transitions: Map.put(state.pending_transitions, transition_id, protocol_version)
    }

    cond do
      transition_id == 0 ->
        execute_transition(transition_id, state)

      protocol_version == 0 ->
        Dave.set_passthrough_mode(state.dave_session, true)

        {state, Payload.dave_transition_ready(transition_id)}

      true ->
        state
    end
  end

  defp handle_event(:dave_execute_transition, data, state) do
    execute_transition(data["transition_id"], state)
  end

  defp handle_event(:dave_prepare_epoch, data, state) do
    if data["epoch"] == 1,
      do: init_dave(data["protocol_version"], state),
      else: state
  end

  defp handle_event(:dave_mls_external_sender, data, state) do
    with session when is_reference(session) <- state.dave_session do
      Dave.set_external_sender(session, data)
    end

    state
  end

  defp handle_event(:dave_mls_proposals, data, state) do
    with <<type, proposals::binary>> when type in 0..1 <- data,
         session when is_reference(session) <- state.dave_session,
         operation_type <- (type == 0 && :append) || :revoke,
         %MapSet{} = user_ids <- state.connected_clients,
         {commit, welcome} when is_binary(commit) <-
           Dave.process_proposals(session, operation_type, proposals, MapSet.to_list(user_ids)) do
      {state, Payload.dave_mls_commit_welcome(commit, welcome)}
    else
      _ -> state
    end
  end

  defp handle_event(
         :dave_mls_announce_commit_transition,
         <<transition_id::big-16, commit::binary>>,
         state
       ) do
    with session when is_reference(session) <- state.dave_session,
         :ok <- Dave.process_commit(session, commit),
         true <- transition_id > 0 do
      pending_transitions =
        Map.put(state.pending_transitions, transition_id, Dave.protocol_version(session))

      {%{state | pending_transitions: pending_transitions},
       Payload.dave_transition_ready(transition_id)}
    else
      :error -> {state, Payload.dave_mls_invalid_commit_welcome(transition_id)}
      _ -> state
    end
  end

  defp handle_event(:dave_mls_welcome, <<transition_id::big-16, welcome::binary>>, state) do
    with session when is_reference(session) <- state.dave_session,
         :ok <- Dave.process_welcome(session, welcome),
         true <- transition_id > 0 do
      pending_transitions =
        Map.put(state.pending_transitions, transition_id, Dave.protocol_version(session))

      {%{state | pending_transitions: pending_transitions},
       Payload.dave_transition_ready(transition_id)}
    else
      :error -> {state, Payload.dave_mls_invalid_commit_welcome(transition_id)}
      _ -> state
    end
  end

  defp handle_event(event, data, state) do
    Logger.debug("UNHANDLED VOICE GATEWAY EVENT #{event}, #{inspect(data)}")
    state
  end

  defp execute_transition(
         transition_id,
         %{dave_session: session, pending_transitions: pending_transitions} = state
       )
       when is_reference(session) and is_map_key(pending_transitions, transition_id) do
    {protocol_version, pending_transitions} = Map.pop(pending_transitions, transition_id)

    if protocol_version == 0, do: Dave.reset(session)

    if transition_id > 0 and protocol_version > 0, do: Dave.set_passthrough_mode(session, true)

    %{state | pending_transitions: pending_transitions}
  end

  defp execute_transition(_transition_id, state), do: state

  defp init_dave(0 = _protocol_version, %{dave_session: nil} = state), do: state

  defp init_dave(0 = _protocol_version, %{dave_session: session} = state) do
    Dave.reset(session)
    Dave.set_passthrough_mode(session, true)
    state
  end

  defp init_dave(protocol_version, %{dave_session: nil} = state) do
    session = Dave.new_session(protocol_version, Me.get().id, state.channel_id)
    key = Dave.get_serialized_key_package(session)
    Voice.update_voice_async(state.voice_pid, state.guild_id, dave_session: session)
    {%{state | dave_session: session}, Payload.dave_mls_key_package(key)}
  end

  defp init_dave(protocol_version, %{dave_session: session} = state) do
    Dave.reinit(session, protocol_version, Me.get().id, state.channel_id)
    key = Dave.get_serialized_key_package(session)
    {state, Payload.dave_mls_key_package(key)}
  end
end
