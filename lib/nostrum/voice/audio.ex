defmodule Nostrum.Voice.Audio do
  @moduledoc false

  require Logger

  alias Nostrum.Error.VoiceError
  alias Nostrum.Struct.VoiceState
  alias Nostrum.Util
  alias Nostrum.Voice
  alias Porcelain.Process, as: Proc

  @encryption_mode "xsalsa20_poly1305"
  @samples_per_frame 960
  @usec_per_frame 20_000
  # How many consecutive packets to send before resting
  @frames_per_burst 10

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
    # 12 byte header + 12 null bytes
    nonce = header <> <<0::96>>
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

    ((@usec_per_frame * @frames_per_burst - (t2 - t1)) / 1000)
    |> trunc()
    |> max(0)
    |> Process.sleep()

    player_loop(voice)
  end

  def send_frames(%VoiceState{} = voice) do
    frames =
      voice.ffmpeg_proc.out
      |> Enum.take(@frames_per_burst)

    voice =
      Enum.reduce(frames, voice, fn f, v ->
        :gen_udp.send(
          v.udp_socket,
          v.ip |> ip_to_tuple(),
          v.port,
          encrypt_packet(v, f)
        )

        %{
          v
          | rtp_sequence: v.rtp_sequence + 1,
            rtp_timestamp: v.rtp_timestamp + @samples_per_frame
        }
      end)

    voice =
      Voice.update_voice(voice.guild_id,
        rtp_sequence: voice.rtp_sequence,
        rtp_timestamp: voice.rtp_timestamp
      )

    if length(frames) < @frames_per_burst do
      Voice.set_speaking(voice, false)
      Proc.stop(voice.ffmpeg_proc)
      Voice.update_voice(voice.guild_id, ffmpeg_proc: nil)
      exit(:normal)
    else
      voice
    end
  end

  def spawn_youtubedl(url) do
    res =
      Porcelain.spawn(
        Application.get_env(:nostrum, :youtubedl, "youtube-dl"),
        [
          "-f",
          "bestaudio",
          "-q",
          "-o",
          "-",
          url
        ],
        out: :stream
      )

    case res do
      {:error, reason} ->
        raise(VoiceError, reason: reason, executable: "youtube-dl")

      proc ->
        proc
    end
  end

  def spawn_ffmpeg(input, type \\ :url) do
    {input_url, stdin} =
      case type do
        :url ->
          {input, <<>>}

        :pipe ->
          {"pipe:0", input}

        :ytdl ->
          %Proc{out: outstream} = spawn_youtubedl(input)

          {"pipe:0", outstream}
      end

    res =
      Porcelain.spawn(
        Application.get_env(:nostrum, :ffmpeg, "ffmpeg"),
        [
          "-i",
          input_url,
          "-ac",
          "2",
          "-ar",
          "48000",
          "-f",
          "s16le",
          "-acodec",
          "libopus",
          "-loglevel",
          "quiet",
          "pipe:1"
        ],
        in: stdin,
        out: :stream
      )

    case res do
      {:error, reason} ->
        raise(VoiceError, reason: reason, executable: "ffmpeg")

      proc ->
        proc
    end
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
        # '1' for request
        1::16,
        # length of rest
        70::16,
        ssrc::32
      >> <>
        (ip |> String.pad_trailing(64, <<0>>)) <>
        <<port::16>>

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
