defmodule Nostrum.Voice do
  @moduledoc """
  Interface for playing and listening to audio through Discord's voice channels.

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

  In addition to playing audio, listening to incoming audio is supported through the
  functions `listen/3` and `start_listen_async/1`.

  ## Voice Without FFmpeg
  If you wish to BYOE (Bring Your Own Encoder), there are a few options.
    - Use `:raw` as `type` for `play/4`
      - Provide the complete list of opus frames as the input
    - Use `:raw_s` as `type` for `play/4`
      - Provide a stateful enumerable of opus frames as input (think GenServer wrapped in `Stream.unfold/2`)
    - Use lower level functions to send opus frames at your leisure
      - Send packets on your own time using `send_frames/2`
  """

  alias Nostrum.Api
  alias Nostrum.Struct.{Channel, Guild, VoiceState, VoiceWSState}
  alias Nostrum.Voice.Audio
  alias Nostrum.Voice.Opus
  alias Nostrum.Voice.Ports
  alias Nostrum.Voice.Session
  alias Nostrum.Voice.Supervisor, as: VoiceSupervisor

  require Logger

  use GenServer

  @typedoc """
  RTP sequence
  """
  @typedoc since: "0.6.0"
  @type rtp_sequence :: non_neg_integer()

  @typedoc """
  RTP timestamp
  """
  @typedoc since: "0.6.0"
  @type rtp_timestamp :: non_neg_integer()

  @typedoc """
  RTP SSRC
  """
  @typedoc since: "0.6.0"
  @type rtp_ssrc :: non_neg_integer()

  @typedoc """
  Opus packet
  """
  @typedoc since: "0.6.0"
  @type opus_packet :: binary()

  @typedoc """
  Tuple with RTP header elements and opus packet
  """
  @typedoc since: "0.6.0"
  @type rtp_opus :: {{rtp_sequence(), rtp_timestamp(), rtp_ssrc()}, opus_packet()}

  @typedoc """
  The type of play input

  The type given to `play/4` determines how the input parameter is interpreted.
  See `play/4` for more information.
  """
  @typedoc since: "0.6.0"
  @type play_type :: :url | :pipe | :ytdl | :stream | :raw | :raw_s

  @typedoc """
  The play input

  The input given to `play/4`, either a compatible URL or binary audio data.
  See `play/4` for more information.
  """
  @typedoc since: "0.6.0"
  @type play_input :: String.t() | binary() | Enum.t()

  @raw_types [:raw, :raw_s]
  @ffm_types [:url, :pipe, :ytdl, :stream]
  @url_types [:url, :ytdl, :stream]

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
  def remove_voice(guild_id, pre_cleanup_args \\ []) do
    GenServer.cast(VoiceStateMap, {:remove, guild_id, pre_cleanup_args})
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
    - `input` - Audio to be played, `t:play_input/0`. Input type determined by `type` parameter.
    - `type` - Type of input, `t:play_type/0` (defaults to `:url`).
      - `:url` Input will be [any url that `ffmpeg` can read](https://www.ffmpeg.org/ffmpeg-protocols.html).
      - `:pipe` Input will be data that is piped to stdin of `ffmpeg`.
      - `:ytdl` Input will be url for `youtube-dl`, which gets automatically piped to `ffmpeg`.
      - `:stream` Input will be livestream url for `streamlink`, which gets automatically piped to `ffmpeg`.
      - `:raw` Input will be an enumerable of raw opus packets. This bypasses `ffmpeg` and all options.
      - `:raw_s` Same as `:raw` but input must be stateful, i.e. calling `Enum.take/2` on `input` is not idempotent.
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
    Volume values between `0.0` and `1.0` act as standard operating range where `0` is off and `1` is max.
    Values greater than `1.0` will add saturation and distortion to the audio.
    Negative values act the same as their position but reverse the polarity of the waveform.

    Having all the ffmpeg audio filters available is *extremely powerful* so it may be worth learning some of them for your use cases.
    If you use any filters to *increase* the playback speed of your audio, it's recommended to set the `:realtime` option to `false`
    because realtime processing is relative to the original playback speed.

  ## Examples

  ```elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "~/music/FavoriteSong.mp3", :url)

  iex> Nostrum.Voice.play(123456789, "~/music/NotFavoriteButStillGoodSong.mp3", :url, volume: 0.5)

  iex> Nostrum.Voice.play(123456789, "~/music/ThisWillBeHeavilyDistorted.mp3", :url, volume: 1000)
  ```
  ```elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> raw_data = File.read!("~/music/sound_effect.wav")

  iex> Nostrum.Voice.play(123456789, raw_data, :pipe)
  ```
  ```elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "https://www.youtube.com/watch?v=b4RJ-QGOtw4", :ytdl,
  ...>   realtime: true, start_pos: "0:17", duration: "30")

  iex> Nostrum.Voice.play(123456789, "https://www.youtube.com/watch?v=0ngcL_5ekXo", :ytdl,
  ...>   filter: "lowpass=f=1200", filter: "highpass=f=300", filter: "asetrate=44100*0.5")
  ```
  ```elixir
  iex> Nostrum.Voice.join_channel(123456789, 420691337)

  iex> Nostrum.Voice.play(123456789, "https://www.twitch.tv/pestily", :stream)

  iex> Nostrum.Voice.play(123456789, "https://youtu.be/LN4r-K8ZP5Q", :stream)
  ```
  """
  @spec play(Guild.id(), play_input(), play_type(), keyword()) :: :ok | {:error, String.t()}
  def play(guild_id, input, type \\ :url, options \\ []) do
    voice = get_voice(guild_id)

    cond do
      not VoiceState.ready_for_rtp?(voice) ->
        {:error, "Must be connected to voice channel to play audio."}

      VoiceState.playing?(voice) ->
        {:error, "Audio already playing in voice channel."}

      true ->
        if is_pid(voice.ffmpeg_proc), do: Ports.close(voice.ffmpeg_proc)

        voice =
          update_voice(guild_id,
            current_url: if(type in @url_types, do: input),
            ffmpeg_proc: if(type in @ffm_types, do: Audio.spawn_ffmpeg(input, type, options)),
            raw_audio: if(type in @raw_types, do: input),
            raw_stateful: type === :raw_s
          )

        set_speaking(voice, true)
        update_voice(guild_id, player_pid: spawn(Audio, :start_player, [voice]))
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

  ```elixir
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
        if is_pid(voice.ffmpeg_proc), do: Ports.close(voice.ffmpeg_proc)
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

  ```elixir
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

  ```elixir
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
        update_voice(guild_id, player_pid: spawn(Audio, :resume_player, [voice]))
        :ok
    end
  end

  @doc """
  Checks if the bot is playing sound in a voice channel.

  ## Parameters
    - `guild_id` - ID of guild to check if audio being played.

  Returns `true` if the bot is currently being played in a voice channel, otherwise `false`.

  ## Examples

  ```elixir
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

  ```elixir
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

  ```elixir
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
    if voice, do: voice.channel_id
  end

  @doc """
  Gets the current URL being played.

  If `play/4` was invoked with type `:url`, `:ytdl`, or `:stream`, this function will return
  the URL given as input last time it was called.

  If `play/4` was invoked with type `:pipe`, `:raw`, or `:raw_s`, this will return `nil`
  as the input is raw audio data, not be a readable URL string.
  """
  @doc since: "0.6.0"
  @spec get_current_url(Guild.id()) :: String.t() | nil
  def get_current_url(guild_id) do
    voice = get_voice(guild_id)
    if voice, do: voice.current_url
  end

  @doc """
  Gets a map of RTP SSRC to user id.

  Within a voice channel, an SSRC (synchronization source) will uniquely map to a
  user id of a user who is speaking.

  If listening to incoming voice packets asynchronously, this function will not be
  needed as the `t:Nostrum.Struct.VoiceWSState.ssrc_map/0` will be available with every event.
  If listening with `listen/3`, this function may be used. It is recommended to
  cache the result of this function and only call it again when you encounter an
  SSRC that is not present in the cached result. This is to reduce excess load on the
  voice websocket and voice state processes.
  """
  @doc since: "0.6.0"
  @spec get_ssrc_map(Guild.id()) :: VoiceWSState.ssrc_map()
  def get_ssrc_map(guild_id) do
    voice = get_voice(guild_id)
    v_ws = Session.get_ws_state(voice.session_pid)
    v_ws.ssrc_map
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
  @spec send_frames(Guild.id(), [opus_packet()]) :: :ok | {:error, String.t()}
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

  Returns a list of tuples of type `t:rtp_opus/0`.

  The inner tuple contains fields from the RTP header and can be matched against
  to retrieve information about the packet such as the SSRC, which identifies the source.
  Note that RTP timestamps are completely unrelated to Unix timestamps.

  If `raw_rtp` is set to `true`, a list of raw RTP packets is returned instead.
  To extract an opus packet from an RTP packet, see `extract_opus_packet/1`.

  This function will block until the specified number of packets is received.
  """
  @doc since: "0.6.0"
  @spec listen(Guild.id(), pos_integer, raw_rtp :: false) :: [rtp_opus()] | {:error, String.t()}
  @spec listen(Guild.id(), pos_integer, raw_rtp :: true) :: [binary] | {:error, String.t()}
  def listen(guild_id, num_packets, raw_rtp \\ false) do
    voice = get_voice(guild_id)

    if VoiceState.ready_for_rtp?(voice) do
      packets = Audio.get_unique_rtp_packets(voice, num_packets)

      if raw_rtp do
        Enum.map(packets, fn {header, payload} -> header <> payload end)
      else
        # credo:disable-for-next-line Credo.Check.Refactor.Nesting
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
  Start asynchronously receiving events for incoming RTP packets for an active voice session.

  This is an alternative to the blocking `listen/3`. Events will be generated asynchronously
  when a user is speaking. See `t:Nostrum.Consumer.voice_incoming_packet/0` for more info.
  """
  @doc since: "0.6.0"
  @spec start_listen_async(Guild.id()) :: :ok | {:error, term()}
  def start_listen_async(guild_id), do: set_udp_active(guild_id, true)

  @doc """
  Stop asynchronously receiving events for incoming RTP packets for an active voice session.
  """
  @doc since: "0.6.0"
  @spec stop_listen_async(Guild.id()) :: :ok | {:error, term()}
  def stop_listen_async(guild_id), do: set_udp_active(guild_id, false)

  defp set_udp_active(guild_id, active?) do
    voice = get_voice(guild_id)

    if VoiceState.ready_for_rtp?(voice) do
      voice.udp_socket |> :inet.setopts([{:active, active?}])
    else
      {:error, "Must be connected to voice channel to alter socket options."}
    end
  end

  @doc """
  Extract the opus packet from the RTP packet received from Discord.

  Incoming voice RTP packets contain a fixed length RTP header and an optional
  RTP header extension, which must be stripped to retrieve the underlying opus packet.
  """
  @doc since: "0.6.0"
  @spec extract_opus_packet(binary) :: opus_packet()
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

  When creating a logical bitstream, ensure that the packets are all from a single SSRC.
  When listening in a channel with multiple speakers, you should be storing the received
  packets in unique buckets for each SSRC so that the multiple audio sources don't become
  jumbled. A single logical bitstream should represent audio data from a single speaker.
  An Ogg physical bitstream (e.g. a file) may be composed of multiple interleaved Ogg
  logical bitstreams as each logical bitstream and its constituent pages contain a unique
  and randomly generated bitstream serial number, but this is a story for another time.

  Assuming you have a list of `t:rtp_opus/0` packets that are not separated by ssrc, you
  may do the following:

  ```elixir
  jumbled_packets
  |> Stream.filter(fn {{_seq, _time, ssrc}, _opus} -> ssrc == particular_ssrc end)
  |> Enum.map(fn {{_seq, _time, _ssrc}, opus} -> opus end)
  |> create_ogg_bitstream()
  ```
  """
  @doc since: "0.5.1"
  @spec create_ogg_bitstream([opus_packet()]) :: [binary]
  defdelegate create_ogg_bitstream(opus_packets), to: Opus

  @doc """
  Pad discontinuous chunks of opus audio with silence.

  This function takes a list of `t:rtp_opus/0`, which is a tuple containing RTP bits and
  opus audio data. It returns a list of opus audio packets. The reason the input has to be in
  the `t:rtp_opus/0` tuple format returned by `listen/3` and async listen events is that the
  RTP packet header contains info on the relative timestamps of incoming packets; the opus
  packets themselves don't contain information relating to timing.

  The Discord client will continue to internally increment the `t:rtp_timestamp()` when the
  user is not speaking such that the duration of pauses can be determined from the RTP packets.
  Bots will typically not behave this way, so if you call this function on audio produced by
  a bot it is very likely that no silence will be inserted.

  The use case of this function is as follows:
  Consider a user speaks for two seconds, pauses for ten seconds, then speaks for another two
  seconds. During the pause, no RTP packets will be received, so if you create a bitstream from
  it, the resulting audio will be both two-second speaking segments consecutively without the
  long pause in the middle. If you wish to preserve the timing of the speaking and include the
  pause, calling this function will interleave the appropriate amount of opus silence packets
  to maintain temporal fidelity.

  Note that the Discord client currently sends about 10 silence packets (200 ms) each time it
  detects end of speech, so creating a bitstream without first padding your audio with this
  function will maintain short silences between speech segments.

  *This function should only be called on a collection of RTP packets from a single SSRC*
  """
  @doc since: "0.6.0"
  @spec pad_opus(nonempty_list(rtp_opus())) :: [opus_packet()]
  defdelegate pad_opus(packets), to: Opus

  @doc false
  def handle_call({:update, guild_id, args}, _from, state) do
    voice =
      state
      |> Map.get(guild_id, VoiceState.new(guild_id: guild_id))
      |> Map.merge(Map.new(args))

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
  def handle_cast({:remove, guild_id, pre_cleanup_args}, state) do
    state
    |> Map.get(guild_id, %{})
    |> Map.merge(Map.new(pre_cleanup_args))
    |> VoiceState.cleanup()

    {:noreply, Map.delete(state, guild_id)}
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
    # Nil-ify ffmpeg_proc so it doesn't get closed when cleanup is called
    if voice.persist_source do
      remove_voice(p.guild_id, ffmpeg_proc: nil)
    else
      remove_voice(p.guild_id)
    end

    update_voice(
      p.guild_id,
      [
        channel_id: p.channel_id,
        session: p.session_id,
        self_mute: p.self_mute,
        self_deaf: p.self_deaf,
        token: nil,
        gateway: nil
      ] ++
        if(voice.persist_source,
          do: [
            ffmpeg_proc: voice.ffmpeg_proc,
            raw_audio: voice.raw_audio,
            raw_stateful: voice.raw_stateful,
            current_url: voice.current_url,
            persist_playback: voice.persist_playback
          ],
          else: []
        )
    )
  end
end
