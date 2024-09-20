defmodule Nostrum.Struct.VoiceState do
  @moduledoc false

  alias Nostrum.Voice.Ports
  alias Nostrum.Voice.Session

  defstruct [
    :guild_id,
    :channel_id,
    :self_mute,
    :self_deaf,
    :gateway,
    :session,
    :token,
    :secret_key,
    :session_pid,
    :ssrc,
    :speaking,
    :ip,
    :port,
    :udp_socket,
    :encryption_mode,
    :rtp_sequence,
    :rtp_timestamp,
    :ffmpeg_proc,
    :raw_audio,
    :raw_stateful,
    :current_url,
    :player_pid,
    :persist_source,
    :persist_playback
  ]

  def new, do: %__MODULE__{}
  def new(params), do: struct(__MODULE__, params)

  def ready_for_ws?(%__MODULE__{} = v) do
    not (is_pid(v.session_pid) or
           is_nil(v.session) or
           is_nil(v.gateway) or
           is_nil(v.token))
  end

  def ready_for_ws?(_), do: false

  def ready_for_rtp?(%__MODULE__{} = v) do
    not (is_nil(v.ip) or
           is_nil(v.port) or
           is_nil(v.ssrc) or
           is_nil(v.secret_key) or
           is_nil(v.udp_socket))
  end

  def ready_for_rtp?(_), do: false

  def playing?(%__MODULE__{} = v) do
    is_pid(v.player_pid) and Process.alive?(v.player_pid)
  end

  def playing?(_), do: false

  def cleanup(%__MODULE__{} = v) do
    if is_pid(v.player_pid),
      do: Process.exit(v.player_pid, :cleanup)

    if is_pid(v.ffmpeg_proc),
      do: Ports.close(v.ffmpeg_proc)

    if is_port(v.udp_socket),
      do: :gen_udp.close(v.udp_socket)

    if is_pid(v.session_pid),
      do: Session.close_connection(v.session_pid)

    :ok
  end

  def cleanup(_), do: :ok
end
