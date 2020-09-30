defmodule Nostrum.Voice.Audio do
  @moduledoc false

  require Logger

  alias Nostrum.Struct.VoiceState
  alias Nostrum.Util
  alias Nostrum.Voice

  @encryption_mode "xsalsa20_poly1305"
  @samples_per_frame 960
  @usec_per_frame 20_000
  @frames_per_burst 10 # How many consecutive packets to send before resting

  def encryption_mode, do: @encryption_mode

  def rtp_header(%VoiceState{} = voice) do
    <<
      0x80::8,
      0x78::8,
      voice.rtp_sequence::16,
      voice.rtp_timestamp::32,
      voice.ssrc::32
    >>
  end

  def encrypt_packet(%VoiceState{} = voice, data) do
    header = rtp_header(voice)
    nonce = header <> <<0::96>> # 12 byte header + 12 null bytes
    header <> Kcl.secretbox(data, nonce, voice.secret_key)
  end

  def open_udp do
    {:ok, socket} =
      :gen_udp.open(0, [
        :binary,
        {:active, false},
        {:reuseaddr, true}
      ])

    socket
  end

  def init_player(voice) do
    Process.sleep(200)
    player_loop(voice)
  end

  def player_loop(voice) do
    t1 = Util.usec_now()
    voice = send_frames(voice)
    t2 = Util.usec_now()

    (((@usec_per_frame * @frames_per_burst) - (t2 - t1)) / 1000)
    |> trunc()
    |> max(0)
    |> Process.sleep()

    player_loop(voice)
  end

  def send_frames(%VoiceState{} = voice) do
    frames =
      voice.ffmpeg_proc.out
      |> Enum.take(@frames_per_burst)

    voice = Enum.reduce(frames, voice, fn f, v ->
      :gen_udp.send(
        v.udp_socket,
        v.ip |> ip_to_tuple(),
        v.port,
        encrypt_packet(v, f)
      )
      %{v |
        rtp_sequence: v.rtp_sequence + 1,
        rtp_timestamp: v.rtp_timestamp + @samples_per_frame
      }
    end)

    voice = Voice.update_guild(voice.guild,
      rtp_sequence: voice.rtp_sequence,
      rtp_timestamp: voice.rtp_timestamp
    )

    if length(frames) < @frames_per_burst do
      Voice.set_speaking(voice, false)
      Voice.update_guild(voice.guild, ffmpeg_proc: nil)
      exit(:normal)
    else
      voice
    end
  end

  def spawn_ffmpeg(input, stream \\ <<>>) do
    {input, stream} =
      case input do
        :stream ->
          {"pipe:0", stream}
        filename ->
          {filename, <<>>}
      end
    Porcelain.spawn(
      Application.get_env(:nostrum, :ffmpeg, "ffmpeg"),
      [
        "-i", input,
        "-ac", "2",
        "-ar", "48000",
        "-f", "s16le",
        "-acodec", "libopus",
        "-loglevel", "quiet",
        "pipe:1"
      ],
      [
        in: stream,
        out: :stream
      ]
    )
  end

  @doc """
  Because UDP is connectionless, we must discover our machine's external
  IP and port used to communicate via the UDP socket to inform the discord
  voice server where to send incoming voice data. This is a bot library, so
  it's unlikely it will be listening to any incoming voice data, but it's
  here for completeness/correctness.
  """
  def discover_ip(socket, ip, port, ssrc) do
    ip_tuple = ip_to_tuple(ip)

    req_packet =
      <<
        1::16,    # '1' for request
        70::16,   # length of rest
        ssrc::32
      >>
      <> (ip |> String.pad_trailing(64, <<0>>))
      <> <<port::16>>

    :ok = :gen_udp.send(socket, ip_tuple, port, req_packet)
    {:ok, res_packet} = :gen_udp.recv(socket, 74)

    {
      ^ip_tuple,
      ^port,
      <<
        2::16,
        70::16,
        ^ssrc::32,
        my_ip::bitstring-size(512),
        my_port::16
      >>
    } = res_packet
    my_ip = my_ip |> String.trim(<<0>>)
    {my_ip, my_port}
  end

  defp ip_to_tuple(ip) do
    {:ok, ip_tuple} =
      ip
      |> String.to_charlist()
      |> :inet_parse.address()
    ip_tuple
  end
end
