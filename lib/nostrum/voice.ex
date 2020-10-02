defmodule Nostrum.Voice do
  @moduledoc """
  Interface for playing audio through Discord's voice channels.

  # Using Discord Voice Channels
  To play sound in discord with Nostrum, you'll need `ffmpeg` to be installed.
  If you don't have the executable `ffmpeg` in the path, the absolute path may
  be configured through config keys `:nostrum, :ffmpeg`.

  A bot may be connected to at most one voice channel per guild. For this reason,
  most of the functions in this module take a guild id, and the resulting action
  will be performed in the given guild's voice channel that the bot is connected to.

  The primary discord gateway responsible for all text based communication relies on
  one websocket connection per shard, where small bots typically only have one shard.
  The discord voice gateway works by establishing a websocket connection per guild/channel.
  After some handshaking on this connection, audio data can be sent over UDP/RTP. Behind
  the scenes the voice websocket connections are implemented nearly the same way the main
  shard websocket connections are, and require no developer intervention.
  """

  alias Nostrum.Api
  alias Nostrum.Struct.{Channel, Guild, VoiceState}
  alias Nostrum.Voice.Audio
  alias Nostrum.Voice.Session
  alias Nostrum.Voice.Supervisor, as: VoiceSupervisor
  alias Porcelain.Process, as: Proc

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

  This function is equivalent to `Nostrum.Api.update_voice_state/4`.
  """
  @spec join_channel(Guild.id(), Channel.id(), boolean, boolean) :: no_return | :ok
  def join_channel(guild_id, channel_id, self_mute \\ false, self_deaf \\ false) do
    Api.update_voice_state(guild_id, channel_id, self_mute, self_deaf)
  end

  @doc """
  Disconnects from the voice channel of the given guild id.

  This function is equivalent to calling `Nostrum.Api.update_voice_state(guild_id, nil)`.
  """
  @spec leave_channel(Guild.id()) :: no_return | :ok
  def leave_channel(guild_id) do
    Api.update_voice_state(guild_id, nil)
  end

  @doc """
  Play sound in the voice channel the bot is in.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be played in.
    - `type` - `:url` if playing file (remote or local), `:pipe` if piping data to stdin.
    - `input` - If `type` `:url`, url or filename. If `type` `:pipe`, raw data to be played.

  Returns `{:error, reason}` if unable to play or a sound is playing, else `:ok`

  If playing sound with the `:pipe` type, the sound must be stopped or paused
  before playing another sound because the ffmpeg process does not close automatically
  when receiving data piped into stdin.

  ## Examples

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, :url, "~/music/FavoriteSong.mp3")
  ```
  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  raw_data = File.read!("~/music/sound_effect.wav")

  Nostrum.Voice.play(123456789, :pipe, raw_data)
  ```
  """
  @spec play(Guild.id(), :url | :pipe, String.t() | binary() | iodata()) :: :ok | {:error, String.t()}
  def play(guild_id, type, input) do
    voice = get_voice(guild_id)

    cond do
      not VoiceState.ready_for_rtp?(voice) ->
        {:error, "Must be connected to voice channel to play audio."}

      VoiceState.playing?(voice) ->
        {:error, "Audio already playing in voice channel."}

      true ->
        unless is_nil(voice.ffmpeg_proc), do: Proc.stop(voice.ffmpeg_pro)
        set_speaking(voice, true)
        voice = update_voice(guild_id, ffmpeg_proc: Audio.spawn_ffmpeg(type, input))
        {:ok, pid} = Task.start(fn -> Audio.init_player(voice) end)
        update_voice(guild_id, player_pid: pid)
        :ok
    end
  end

  @doc """
  Stop the current sound being in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be stopped in.

  Returns `{:error, reason}` if unable to stop or no sound is playing, else `:ok`

  If a sound played from a file has already completed, this function does not need
  to be called. If playing from a stream, this function musted be called before another
  sound be played in the specified guild's voice channel.

  ## Examples

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, :url, "http://brandthill.com/files/weird_dubstep_noises.mp3")

  Nostrum.Voice.stop(123456789)
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
        Proc.stop(voice.ffmpeg_proc)
        :ok
    end
  end

  @doc """
  Pause the current sound being played in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be paused in.

  Returns `{:error, reason}` if unable to pause or no sound is playing, else `:ok`

  This function is similar to `stop/1`, except that the sound may be
  resumed after being paused.

  ## Examples

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, :url, "~/files/twelve_hour_loop_of_waterfall_sounds.mp3")

  Nostrum.Voice.pause(123456789)
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
  Resume the current sound being played in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be resumed in.

  Returns `{:error, reason}` if unable to resume or no sound has been paused, otherwise returns `:ok`

  This function is used to resume a sound that had previously been paused.

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, :url, "~/stuff/Toto - Africa (Bass Boosted)")

  Nostrum.Voice.pause(123456789)

  Nostrum.Voice.resume(123456789)
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

      is_nil(voice.ffmpeg_proc) ->
        {:error, "Audio must be paused to resume."}

      true ->
        set_speaking(voice, true)
        {:ok, pid} = Task.start(fn -> Audio.player_loop(voice) end)
        update_voice(guild_id, player_pid: pid)
        :ok
    end
  end

  @doc false
  def set_speaking(%VoiceState{} = voice, speaking) do
    Session.set_speaking(voice.session_pid, speaking)
  end

  @doc false
  def set_speaking(guild_id, speaking) do
    get_voice(guild_id) |> set_speaking(speaking)
  end

  @doc false
  def handle_call({:update, guild_id, args}, _from, state) do
    voice =
      state
      |> Map.get(guild_id, VoiceState.new(guild_id: guild_id))
      |> Map.merge(Enum.into(args, %{}))

    state = Map.put(state, guild_id, voice)
    start_if_ready(voice)
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
end
