defmodule Nostrum.Voice do
  @moduledoc """
  Interface for playing audio through Discord's voice channels.

  # Using Discord Voice Channels
  To play sound in Discord with Nostrum, you'll need `ffmpeg` to be installed.
  If you don't have the executable `ffmpeg` in the path, the absolute path may
  be configured through config keys `:nostrum, :ffmpeg`. If you don't want to use
  ffmpeg, read on to the next section.

  A bot may be connected to at most one voice channel per guild. For this reason,
  most of the functions in this module take a guild id, and the resulting action
  will be performed in the given guild's voice channel that the bot is connected to.

  The primary Discord gateway responsible for all text based communication relies on
  one websocket connection per shard, where small bots typically only have one shard.
  The Discord voice gateways work by establishing a websocket connection per guild/channel.
  After some handshaking on this connection, audio data can be sent over UDP/RTP. Behind
  the scenes the voice websocket connections are implemented nearly the same way the main
  shard websocket connections are, and require no developer intervention.

  ## Voice Without FFmpeg
  If you wish to BYOE (Bring Your Own Encoder), there are a few options.
    - Use `:raw` as `type` for `Nostrum.Voice.play/4`
      - Provide the complete list of opus frames as the input
    - Use `:raw_s` as `type` for `Nostrum.Voice.play/4`
      - Provide a stateful enumerable of opus frames as input (think GenServer wrapped in `Stream.unfold/2`)
    - Use lower level functions to send opus frames at your leisure
      - Send packets on your own time using `Nostrum.Voice.send_frames/2`
  """

  alias Nostrum.Api
  alias Nostrum.Struct.{Channel, Guild, VoiceState}
  alias Nostrum.Voice.Audio
  alias Nostrum.Voice.Opus
  alias Nostrum.Voice.Ports
  alias Nostrum.Voice.Session
  alias Nostrum.Voice.Supervisor, as: VoiceSupervisor

  require Logger

  use GenServer

  @doc false
  def start_link(_args) do
    GenServer.start_link(__MODULE__, %{}, name: VoiceStateMap)
  end

  @doc false
  def init(args) do
    {:ok, args}
  end

  @doc false
  def update_voice(guild_id, args \\ []) do
    GenServer.call(VoiceStateMap, {:update, guild_id, args})
  end

  @doc false
  def get_voice(guild_id) do
    GenServer.call(VoiceStateMap, {:get, guild_id})
  end

  @doc false
  def remove_voice(guild_id) do
    GenServer.call(VoiceStateMap, {:remove, guild_id})
  end

  @doc """
  Joins or moves the bot to a voice channel.

  This function calls `Nostrum.Api.update_voice_state/4`.

  The fifth argument `persist` defaults to `true`. When true, if calling `join_channel/5`
  while already in a different channel in the same guild, the audio source will be persisted
  in the new channel. If the audio is actively playing at the time of changing channels,
  it will resume playing automatically upon joining. If there is an active audio source
  that has been paused before changing channels, the audio will be able to be resumed manually if
  `resume/1` is called.

  If `persist` is set to false, the audio source will be destroyed before changing channels.
  The same effect is achieved by calling `stop/1` or `leave_channel/1` before `join_channel/5`
  """
  @spec join_channel(Guild.id(), Channel.id(), boolean, boolean, boolean) :: no_return | :ok
  def join_channel(
        guild_id,
        channel_id,
        self_mute \\ false,
        self_deaf \\ false,
        persist \\ true
      ) do
    with %VoiceState{} = voice <- get_voice(guild_id) do
      update_voice(guild_id,
        persist_source: persist,
        persist_playback: persist and VoiceState.playing?(voice)
      )
    end

    Api.update_voice_state(guild_id, channel_id, self_mute, self_deaf)
  end

  @doc """
  Leaves the voice channel of the given guild id.

  This function is equivalent to calling `Nostrum.Api.update_voice_state(guild_id, nil)`.
  """
  @spec leave_channel(Guild.id()) :: no_return | :ok
  def leave_channel(guild_id) do
    Api.update_voice_state(guild_id, nil)
  end

  @doc """
  Plays sound in the voice channel the bot is in.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be played in.
    - `input` - Audio to be played. Type of `input` determined by `type` parameter.
    - `type` - Type of input (defaults to `:url`).
      - `:url` Input will be [any url that `ffmpeg` can read](https://www.ffmpeg.org/ffmpeg-protocols.html).
      - `:pipe` Input will be data that is piped to stdin of `ffmpeg`.
      - `:ytdl` Input will be url for `youtube-dl`, which gets automatically piped to `ffmpeg`.
      - `:stream` Input will be livestream url for `streamlink`, which gets automatically piped to `ffmpeg`.
      - `:raw` Input will be an enumarable of raw opus frames. This bypasses `ffmpeg` and all options.
      - `:raw_s` Same as `:raw` but input must be stateful, i.e. calling `Enum.take/2` on input is not idempotent.
    - `options` - See options section below.


  Returns `{:error, reason}` if unable to play or a sound is playing, else `:ok`.

  ## Options
    - `:start_pos` (string) - The start position of the audio to be played. Defaults to beginning.
    - `:duration` (string) - The duration to of the audio to be played . Defaults to entire duration.
    - `:realtime` (boolean) - Make ffmpeg process the input in realtime instead of as fast as possible. Defaults to true.
    - `:volume` (number) - The output volume of the audio. Default volume is 1.0.
    - `:filter` (string) - Filter(s) to be applied to the audio. No filters applied by default.

    The values of `:start_pos` and `:duration` can be [any time duration that ffmpeg can read](https://ffmpeg.org/ffmpeg-utils.html#Time-duration).
    The `:filter` can be used multiple times in a single call (see examples).
    The values of `:filter` can be [any audio filters that ffmpeg can read](https://ffmpeg.org/ffmpeg-filters.html#Audio-Filters).
    Filters will be applied in order and can be as complex as you want. The world is your oyster!

    Note that using the `:volume` option is shortcut for the "volume" filter, and will be added to the end of the filter chain, acting as a master volume.
    Volume values between `0.0` and `1.0` act as standard oparating range where `0` is off and `1` is max.
    Values greater than `1.0` will add saturation and distortion to the audio.
    Negative values act the same as their position but reverse the polarity of the waveform.

    Having all the ffmpeg audio filters available is *extremely powerful* so it may be worth learning some of them for your use cases.
    If you use any filters to *increase* the playback speed of your audio, it's recommended to set the `:realtime` option to `false`
    because realtime processing is relative to the original playback speed.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "~/music/FavoriteSong.mp3", :url)

  iex> Nostrum.Voice.play(123456789, "~/music/NotFavoriteButStillGoodSong.mp3", :url, volume: 0.5)

  iex> Nostrum.Voice.play(123456789, "~/music/ThisWillBeHeavilyDistorted.mp3", :url, volume: 1000)
  ```
  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> raw_data = File.read!("~/music/sound_effect.wav")

  iex> Nostrum.Voice.play(123456789, raw_data, :pipe)
  ```
  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "https://www.youtube.com/watch?v=b4RJ-QGOtw4", :ytdl,
  ...>   realtime: true, start_pos: "0:17", duration: "30")

  iex> Nostrum.Voice.play(123456789, "https://www.youtube.com/watch?v=0ngcL_5ekXo", :ytdl,
  ...>   filter: "lowpass=f=1200", filter: "highpass=f=300", filter: "asetrate=44100*0.5")
  ```
  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "https://www.twitch.tv/pestily", :stream)

  iex> Nostrum.Voice.play(123456789, "https://youtu.be/LN4r-K8ZP5Q", :stream)
  ```
  """
  @spec play(
          Guild.id(),
          String.t() | iodata() | Enum.t(),
          :url | :pipe | :ytdl | :stream | :raw | :raw_s,
          keyword()
        ) ::
          :ok | {:error, String.t()}
  def play(guild_id, input, type \\ :url, options \\ []) do
    voice = get_voice(guild_id)

    cond do
      not VoiceState.ready_for_rtp?(voice) ->
        {:error, "Must be connected to voice channel to play audio."}

      VoiceState.playing?(voice) ->
        {:error, "Audio already playing in voice channel."}

      true ->
        unless is_nil(voice.ffmpeg_proc), do: Ports.close(voice.ffmpeg_proc)
        set_speaking(voice, true)

        {ffmpeg_proc, raw_audio, raw_stateful} =
          case type do
            :raw -> {nil, input, false}
            :raw_s -> {nil, input, true}
            _ffmpeg -> {Audio.spawn_ffmpeg(input, type, options), nil, false}
          end

        voice =
          update_voice(guild_id,
            ffmpeg_proc: ffmpeg_proc,
            raw_audio: raw_audio,
            raw_stateful: raw_stateful
          )

        {:ok, pid} = Task.start(fn -> Audio.start_player(voice) end)
        update_voice(guild_id, player_pid: pid)
        :ok
    end
  end

  @doc """
  Stops the current sound being played in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be stopped in.

  Returns `{:error, reason}` if unable to stop or no sound is playing, else `:ok`.

  If a sound has finished playing, this function does not need to be called to start
  playing another sound.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "http://brandthill.com/files/weird_dubstep_noises.mp3")

  iex> Nostrum.Voice.stop(123456789)
  ```
  """
  @spec stop(Guild.id()) :: :ok | {:error, String.t()}
  def stop(guild_id) do
    voice = get_voice(guild_id)

    cond do
      not VoiceState.ready_for_rtp?(voice) ->
        {:error, "Must be connected to voice channel to stop audio."}

      not VoiceState.playing?(voice) ->
        {:error, "Audio must be playing to stop."}

      true ->
        set_speaking(voice, false)
        Process.exit(voice.player_pid, :stop)
        unless is_nil(voice.ffmpeg_proc), do: Ports.close(voice.ffmpeg_proc)
        :ok
    end
  end

  @doc """
  Pauses the current sound being played in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be paused in.

  Returns `{:error, reason}` if unable to pause or no sound is playing, else `:ok`.

  This function is similar to `stop/1`, except that the sound may be
  resumed after being paused.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "~/files/twelve_hour_loop_of_waterfall_sounds.mp3")

  iex> Nostrum.Voice.pause(123456789)
  ```
  """
  @spec pause(Guild.id()) :: :ok | {:error, String.t()}
  def pause(guild_id) do
    voice = get_voice(guild_id)

    cond do
      not VoiceState.ready_for_rtp?(voice) ->
        {:error, "Must be connected to voice channel to pause audio."}

      not VoiceState.playing?(voice) ->
        {:error, "Audio must be playing to pause."}

      true ->
        set_speaking(voice, false)
        Process.exit(voice.player_pid, :pause)
        :ok
    end
  end

  @doc """
  Resumes playing the current paused sound in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be resumed in.

  Returns `{:error, reason}` if unable to resume or no sound has been paused, otherwise returns `:ok`.

  This function is used to resume a sound that had previously been paused.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "~/stuff/Toto - Africa (Bass Boosted)")

  iex> Nostrum.Voice.pause(123456789)

  iex> Nostrum.Voice.resume(123456789)
  ```
  """
  @spec resume(Guild.id()) :: :ok | {:error, String.t()}
  def resume(guild_id) do
    voice = get_voice(guild_id)

    cond do
      not VoiceState.ready_for_rtp?(voice) ->
        {:error, "Must be connected to voice channel to resume audio."}

      VoiceState.playing?(voice) ->
        {:error, "Audio already playing in voice channel."}

      is_nil(voice.ffmpeg_proc) and is_nil(voice.raw_audio) ->
        {:error, "Audio must be paused to resume."}

      true ->
        set_speaking(voice, true)
        {:ok, pid} = Task.start(fn -> Audio.resume_player(voice) end)
        update_voice(guild_id, player_pid: pid)
        :ok
    end
  end

  @doc """
  Checks if the bot is playing sound in a voice channel.

  ## Parameters
    - `guild_id` - ID of guild to check if audio being played.

  Returns `true` if the bot is currently being played in a voice channel, otherwise `false`.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "https://a-real-site.biz/RickRoll.m4a")

  iex> Nostrum.Voice.playing?(123456789)
  true

  iex> Nostrum.Voice.pause(123456789)

  iex> Nostrum.Voice.playing?(123456789)
  false
  ```
  """
  @spec playing?(Guild.id()) :: boolean
  def playing?(guild_id) do
    get_voice(guild_id) |> VoiceState.playing?()
  end

  @doc """
  Checks if the connection is up and ready to play audio.

  ## Parameters
    - `guild_id` - ID of guild to check if voice connection is up.

  Returns `true` if the bot is connected to a voice channel, otherwise `false`.

  This function does not check if audio is already playing. For that, use `playing?/1`.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.ready?(123456789)
  true

  iex> Nostrum.Voice.leave_channel(123456789)

  iex> Nostrum.Voice.ready?(123456789)
  false
  ```
  """
  @spec ready?(Guild.id()) :: boolean
  def ready?(guild_id) do
    get_voice(guild_id) |> VoiceState.ready_for_rtp?()
  end

  @doc """
  Gets the id of the voice channel that the bot is connected to.

  ## Parameters
    - `guild_id` - ID of guild that the resultant channel belongs to.

  Returns the `channel_id` for the channel the bot is connected to, otherwise `nil`.

  ## Examples

  ```Elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.get_channel(123456789)
  420691337

  iex> Nostrum.Voice.leave_channel(123456789)

  iex> Nostrum.Voice.get_channel(123456789)
  nil
  ```
  """
  @spec get_channel_id(Guild.id()) :: Channel.id()
  def get_channel_id(guild_id) do
    voice = get_voice(guild_id)
    if voice, do: voice.channel_id, else: nil
  end

  @doc false
  def set_speaking(%VoiceState{} = voice, speaking, timed_out \\ false) do
    Session.set_speaking(voice.session_pid, speaking, timed_out)
  end

  @doc """
  Low-level. Set speaking flag in voice channel.

  This function does not need to be called unless you are sending audio frames
  directly using `Nostrum.Voice.send_frames/2`.
  """
  @doc since: "0.5.0"
  @spec set_is_speaking(Guild.id(), boolean) :: :ok
  def set_is_speaking(guild_id, speaking), do: get_voice(guild_id) |> set_speaking(speaking)

  @doc """
  Low-level. Send pre-encoded audio packets directly.

  Speaking should be set to true via `Nostrum.Voice.set_is_speaking/2` before sending frames.

  Opus frames will be encrypted and prefixed with the appropriate RTP header and sent immediately.
  The length of `frames` depends on how often you wish to send a sequence of frames.
  A single frame contains 20ms of audio. Sending more than 50 frames (1 second of audio)
  in a single function call may result in inconsistent playback rates.

  `Nostrum.Voice.playing?/1` will not return accurate values when using `send_frames/2`
  instead of `Nostrum.Voice.play/4`
  """
  @doc since: "0.5.0"
  @spec send_frames(Guild.id(), [binary]) :: :ok | {:error, String.t()}
  def send_frames(guild_id, frames) when is_list(frames) do
    voice = get_voice(guild_id)

    if VoiceState.ready_for_rtp?(voice) do
      Audio.send_frames(frames, voice)
      :ok
    else
      {:error, "Must be connected to voice channel to send frames."}
    end
  end

  @doc """
  Low-level. Manually connect to voice websockets gateway.

  This function should only be called if config option `:voice_auto_connect` is set to `false`.
  By default Nostrum will automatically create a voice gateway when joining a channel.
  """
  @doc since: "0.5.0"
  @spec connect_to_gateway(Guild.id()) :: :ok | {:error, String.t()}
  def connect_to_gateway(guild_id) do
    voice = get_voice(guild_id)

    cond do
      VoiceState.ready_for_ws?(voice) ->
        VoiceSupervisor.create_session(voice)
        :ok

      is_nil(voice) ->
        {:error, "Must be in voice channel to connect to gateway."}

      true ->
        {:error, "Voice gateway connection already open."}
    end
  end

  @doc """
  Listen for incoming voice RTP packets.

  ## Parameters
    - `guild_id` - ID of guild that the bot is listening to.
    - `num_packets` - Number of packets to wait for.
    - `raw_rtp` - Whether to return raw RTP packets. Defaults to `false`.

  Returns a list of tuples in the form `{{rtp_seq, rtp_time, rtp_ssrc}, opus_packet}`.

  The inner tuple contains fields from the RTP header and can be matched against
  to retrieve information about the packet such as the SSRC, which identifies the source.
  Note that RTP timestamps are completely unrelated to Unix timestamps.

  If `raw_rtp` is set to `true`, a list of raw RTP packets is returned instead.
  To extract an opus packet from an RTP packet, see `extract_opus_packet/1`.

  This function will block until the specified number of packets is received.
  """
  @doc since: "0.6.0"
  @spec listen(Guild.id(), pos_integer, boolean) ::
          [{{integer, integer, integer}, binary}] | [binary] | {:error, String.t()}
  def listen(guild_id, num_packets, raw_rtp \\ false) do
    voice = get_voice(guild_id)

    if VoiceState.ready_for_rtp?(voice) do
      packets = Audio.get_unique_rtp_packets(voice, num_packets)

      if raw_rtp do
        Enum.map(packets, fn {header, payload} -> header <> payload end)
      else
        Enum.map(packets, fn {header, payload} ->
          <<_::16, seq::integer-16, time::integer-32, ssrc::integer-32>> = header
          opus = Opus.strip_rtp_ext(payload)
          {{seq, time, ssrc}, opus}
        end)
      end
    else
      {:error, "Must be connected to voice channel to listen for incoming data."}
    end
  end

  @doc """
  Extract the opus packet from the RTP packet received from Discord.

  Incoming voice RTP packets contain a fixed length RTP header and an optional 
  RTP header extension, which must be stripped to retrieve the underlying opus packet.
  """
  @doc since: "0.6.0"
  @spec extract_opus_packet(binary) :: binary
  def extract_opus_packet(packet) do
    <<_header::96, payload::binary>> = packet
    Opus.strip_rtp_ext(payload)
  end

  @doc """
  Create a complete Ogg logical bitstream from a list of Opus packets.

  This function takes a list of opus packets and returns a list of Ogg
  encapsulated Opus pages for a single Ogg logical bitstream.

  It is highly recommended to learn about the Ogg container format to
  understand how to use the data.

  To get started, assuming you have a list of evenly temporally spaced
  and consecutive opus packets from a single source that you want written
  to a file, you can run the following:

  ```elixir
  bitstream =
    opus_packets
    |> create_ogg_bitstream()
    |> :binary.list_to_bin()

  File.write!("my_recording.ogg", bitstream)
  ```
  """
  @doc since: "0.5.1"
  @spec create_ogg_bitstream(list(binary)) :: list(binary)
  def create_ogg_bitstream(opus_packets) do
    Opus.create_ogg_bitstream(opus_packets)
  end

  @doc false
  def handle_call({:update, guild_id, args}, _from, state) do
    voice =
      state
      |> Map.get(guild_id, VoiceState.new(guild_id: guild_id))
      |> Map.merge(Enum.into(args, %{}))

    state = Map.put(state, guild_id, voice)

    if Application.get_env(:nostrum, :voice_auto_connect, true),
      do: start_if_ready(voice)

    {:reply, voice, state}
  end

  @doc false
  def handle_call({:get, guild_id}, _from, state) do
    {:reply, Map.get(state, guild_id), state}
  end

  @doc false
  def handle_call({:remove, guild_id}, _from, state) do
    state[guild_id] |> VoiceState.cleanup()
    VoiceSupervisor.end_session(guild_id)

    {:reply, true, Map.delete(state, guild_id)}
  end

  @doc false
  def start_if_ready(%VoiceState{} = voice) do
    if VoiceState.ready_for_ws?(voice) do
      VoiceSupervisor.create_session(voice)
    end
  end

  @doc false
  def on_channel_join_new(p) do
    update_voice(p.guild_id,
      channel_id: p.channel_id,
      session: p.session_id,
      self_mute: p.self_mute,
      self_deaf: p.self_deaf
    )
  end

  @doc false
  def on_channel_join_change(p, voice) do
    v_ws = Session.get_ws_state(voice.session_pid)

    # On the off-chance that we receive Voice Server Update first:
    {new_token, new_gateway} =
      if voice.token == v_ws.token do
        # Need to reset
        {nil, nil}
      else
        # Already updated
        {voice.token, voice.gateway}
      end

    %{
      ffmpeg_proc: ffmpeg_proc,
      raw_audio: raw_audio,
      raw_stateful: raw_stateful,
      persist_source: persist_source,
      persist_playback: persist_playback
    } = voice

    # Nil-ify ffmpeg_proc so it doesn't get closed when cleanup is called
    if persist_source, do: update_voice(p.guild_id, ffmpeg_proc: nil)

    remove_voice(p.guild_id)

    fields =
      [
        channel_id: p.channel_id,
        session: p.session_id,
        self_mute: p.self_mute,
        self_deaf: p.self_deaf,
        token: new_token,
        gateway: new_gateway
      ] ++
        if persist_source,
          do: [
            ffmpeg_proc: ffmpeg_proc,
            raw_audio: raw_audio,
            raw_stateful: raw_stateful,
            persist_playback: persist_playback
          ],
          else: []

    update_voice(p.guild_id, fields)
  end
end
