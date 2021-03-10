defmodule Nostrum.Voice.Payload do
  @moduledoc false

  alias Nostrum.Cache.Me
  alias Nostrum.Constants
  alias Nostrum.Voice.Audio
  alias Nostrum.Struct.{VoiceState, VoiceWSState}

  require Logger

  def heartbeat_payload do
    DateTime.utc_now()
    |> DateTime.to_unix()
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
      session_id: state.session
    }
    |> build_payload("RESUME")
  end

  def select_protocol_payload(ip, port) do
    %{
      protocol: "udp",
      data: %{
        address: ip,
        port: port,
        mode: Audio.encryption_mode()
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
        timeout: timed_out
      }
    }
  end

  def build_payload(data, opcode_name) do
    opcode = Constants.voice_opcode_from_name(opcode_name)

    %{op: opcode, d: data}
    |> Poison.encode!()
  end
end
