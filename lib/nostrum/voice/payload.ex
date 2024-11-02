defmodule Nostrum.Voice.Payload do
  @moduledoc false

  alias Nostrum.Cache.Me
  alias Nostrum.Constants
  alias Nostrum.Struct.VoiceState
  alias Nostrum.Struct.VoiceWSState

  require Logger

  # All functions in this module that end with a call to `build_payload/1`
  # with an all-caps string return JSON payloads which are response messages
  # to incoming voice websocket messages.
  # Other functions which return a map with keys `:t` and `:d` are for
  # generating voice-related events to be consumed by a Consumer process.

  def heartbeat_payload(%VoiceWSState{} = state) do
    %{
      t: DateTime.utc_now() |> DateTime.to_unix(:millisecond),
      seq_ack: state.seq
    }
    |> build_payload("HEARTBEAT")
  end

  def identify_payload(%VoiceWSState{} = state) do
    %{
      server_id: state.guild_id,
      user_id: Me.get().id,
      token: state.token,
      session_id: state.session
    }
    |> build_payload("IDENTIFY")
  end

  def resume_payload(%VoiceWSState{} = state) do
    %{
      server_id: state.guild_id,
      token: state.token,
      session_id: state.session,
      seq_ack: state.seq
    }
    |> build_payload("RESUME")
  end

  def select_protocol_payload(ip, port, mode) do
    %{
      protocol: "udp",
      data: %{
        address: ip,
        port: port,
        mode: "#{mode}"
      }
    }
    |> build_payload("SELECT_PROTOCOL")
  end

  def speaking_payload(%VoiceState{} = voice) do
    %{
      ssrc: voice.ssrc,
      delay: 0,
      speaking: if(voice.speaking, do: 1, else: 0)
    }
    |> build_payload("SPEAKING")
  end

  def speaking_update_payload(%VoiceState{} = voice, timed_out \\ false) do
    %{
      t: :VOICE_SPEAKING_UPDATE,
      d: %{
        guild_id: voice.guild_id,
        channel_id: voice.channel_id,
        speaking: voice.speaking,
        current_url: voice.current_url,
        timed_out: timed_out
      }
    }
  end

  def voice_ready_payload(%VoiceState{} = voice) do
    %{
      t: :VOICE_READY,
      d: %{
        guild_id: voice.guild_id,
        channel_id: voice.channel_id
      }
    }
  end

  def voice_incoming_packet(payload) do
    %{
      t: :VOICE_INCOMING_PACKET,
      d: payload
    }
  end

  def build_payload(data, opcode_name) do
    opcode = Constants.voice_opcode_from_name(opcode_name)

    %{op: opcode, d: data}
    |> Jason.encode_to_iodata!()
  end
end
