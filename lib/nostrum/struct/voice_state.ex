defmodule Nostrum.Struct.VoiceState do
  @moduledoc false

  alias Nostrum.Voice.Session
  alias Porcelain.Process, as: Proc

  defstruct [
    :guild_id,
    :channel_id,
    :gateway,
    :session,
    :token,
    :secret_key,
    :session_pid,
    :ssrc,
    :speaking,
    :ip,
    :port,
    :mode,
    :udp_socket,
    :rtp_sequence,
    :rtp_timestamp,
    :ffmpeg_proc,
    :player_pid
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
    unless is_nil(v.player_pid) do
      if Process.alive?(v.player_pid) do
        Process.exit(v.player_pid, :cleanup)
      end
    end

    unless is_nil(v.ffmpeg_proc) do
      if Proc.alive?(v.ffmpeg_proc) do
        Proc.stop(v.ffmpeg_proc)
      end
    end

    unless is_nil(v.udp_socket) do
      :gen_udp.close(v.udp_socket)
    end

    unless is_nil(v.session_pid) do
      Session.close_connection(v.session_pid)
    end

    :ok
  end

  def cleanup(_), do: :ok
end
