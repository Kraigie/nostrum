defmodule Nostrum.Voice.Payload do
  @moduledoc false

  alias Nostrum.Cache.Me
  alias Nostrum.Struct.VoiceState
  alias Nostrum.Struct.VoiceWSState

  require Logger

  require Nostrum.Voice.Macros

  import Nostrum.Voice.Macros,
    only: [def_json_payload: 2, def_binary_payload: 2, def_dispatch_payload: 2]

  # Functions in this module create payloads to be sent to Voice WS gateway or to dispatch to event consumers.
  # For all functions defined with def_(json|binary|dispatch)_payload, opcode and/or event_type is inferred from function name.
  #
  # def_json_payload will wrap the returned map in the gateway format with inferred opcode and encode as JSON
  # def_binary_payload will prefix the returned binary with the inferred opcode
  # def_dispatch_payload will wrap returned term in map with keys `:t` and `:d` with inferred event type for dispatch handling
  #
  # Voice Gateway Opcodes are defined here: https://docs.discord.com/developers/topics/opcodes-and-status-codes#voice
  # Dispatch payloads are specific to Nostrum and are for consuming voice related events like other gateway events

  ## Standard Voice gateway payloads

  def_json_payload heartbeat(%VoiceWSState{} = state) do
    %{
      t: DateTime.utc_now() |> DateTime.to_unix(:millisecond),
      seq_ack: state.seq
    }
  end

  def_json_payload identify(%VoiceWSState{} = state) do
    %{
      server_id: state.guild_id,
      user_id: Me.get().id,
      max_dave_protocol_version: Dave.max_protocol_version(),
      token: state.token,
      session_id: state.session
    }
  end

  def_json_payload resume(%VoiceWSState{} = state) do
    %{
      server_id: state.guild_id,
      token: state.token,
      session_id: state.session,
      seq_ack: state.seq
    }
  end

  def_json_payload select_protocol(ip, port, mode) do
    %{
      protocol: "udp",
      data: %{
        address: ip,
        port: port,
        mode: mode
      }
    }
  end

  def_json_payload speaking(%VoiceState{} = voice) do
    %{
      ssrc: voice.ssrc,
      delay: 0,
      speaking: if(voice.speaking, do: 1, else: 0)
    }
  end

  # Custom dispatch events for consumers

  def_dispatch_payload voice_speaking_update(%VoiceState{} = voice, timed_out \\ false) do
    %{
      guild_id: voice.guild_id,
      channel_id: voice.channel_id,
      speaking: voice.speaking,
      current_url: voice.current_url,
      timed_out: timed_out
    }
  end

  def_dispatch_payload voice_ready(%VoiceState{} = voice) do
    %{
      guild_id: voice.guild_id,
      channel_id: voice.channel_id
    }
  end

  def_dispatch_payload voice_incoming_packet({{_seq, _time, _ssrc}, _opus} = packet) do
    packet
  end

  ## DAVE specific payloads, both JSON and binary

  def_json_payload dave_transition_ready(transition_id) do
    %{transition_id: transition_id}
  end

  def_json_payload dave_mls_invalid_commit_welcome(transition_id) do
    %{transition_id: transition_id}
  end

  def_binary_payload dave_mls_key_package(key) do
    key
  end

  def_binary_payload dave_mls_commit_welcome(commit, welcome) do
    commit <> (welcome || <<>>)
  end
end
