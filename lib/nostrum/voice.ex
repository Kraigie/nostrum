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
  def update_guild(guild, args \\ []) do
    GenServer.call(VoiceStateMap, {:update, guild, args})
  end

  @doc false
  def get_guild(guild) do
    GenServer.call(VoiceStateMap, {:get, guild})
  end
  @doc false
  def remove_guild(guild) do
    GenServer.cast(VoiceStateMap, {:remove, guild})
  end

  @doc """
  Joins or moves the bot to a voice channel.

  This function is equivalent to `t:Nostrum.Api.update_voice_state/4`.
  """
  @spec join_channel(Guild.id(), Channel.id(), boolean, boolean) :: no_return | :ok
  def join_channel(guild_id, channel_id, self_mute \\ false, self_deaf \\ false) do
    Api.update_voice_state(guild_id, channel_id, self_mute, self_deaf)
  end

  @doc """
  Disconnects from the voice channel of the given guild id.

  This function is equivalent to calling `t:Nostrum.Api.update_voice_state(guild_id, nil)`.
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
    - `filename` - Filename of file to be played, or `:stream` if piping data.
    - `stream` - Data to be piped into ffmpeg if `filename` was set to `:stream`

  Returns `:noop` if unable to play or a sound is playing, otherwise returns `:ok`

  If playing sound via the `stream` parameter, the sound must be stopped or paused
  before playing another sound because the ffmpeg process does not close automatically
  when receiving data piped into stdin.

  ## Examples

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, "~/music/FavoriteSong.mp3")
  ```
  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  raw_data = File.read!("~/music/sound_effect.wav")

  Nostrum.Voice.play(123456789, :stream, raw_data)
  ```
  """
  @spec play(Guild.id(), String.t() | :stream, binary() | iodata()) :: :noop | :ok
  def play(guild_id, filename, stream \\ <<>>) do
    voice = get_guild(guild_id)
    if VoiceState.ready_for_rtp?(voice) and not VoiceState.playing?(voice) do
      unless is_nil(voice.ffmpeg_proc), do: Proc.stop(voice.ffmpeg_pro)
      set_speaking(voice, true)
      voice = update_guild(guild_id, ffmpeg_proc: Audio.spawn_ffmpeg(filename, stream))
      {:ok, pid} = Task.start(fn -> Audio.init_player(voice) end)
      update_guild(guild_id, player_pid: pid)
      :ok
    else
      :noop
    end
  end

  @doc """
  Stop the current sound being in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be stopped in.

  Returns `:noop` if unable to stop or no sound is playing, otherwise returns `:ok`

  If a sound played from a file has already completed, this function does not need
  to be called. If playing from a stream, this function musted be called before another
  sound be played in the specified guild's voice channel.

  ## Examples

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, "~/things/4_hour_nightcore_mix.ogg")

  Nostrum.Voice.stop(123456789)
  ```
  """
  @spec stop(Guild.id()) :: :noop | :ok
  def stop(guild_id) do
    voice = get_guild(guild_id)
    if VoiceState.playing?(voice) do
      set_speaking(voice, false)
      Process.exit(voice.player_pid, :stop)
      Porcelain.Process.stop(voice.ffmpeg_proc)
      :ok
    else
      :noop
    end
  end

  @doc """
  Pause the current sound being played in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be paused in.

  Returns `:noop` if unable to pause or no sound is playing, otherwise returns `:ok`

  This function is similar to `t:Nostrum.Voice.stop/1`, except that the sound may be
  resumed after being paused.

  ## Examples

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, "~/files/twelve_hour_loop_of_waterfall_sounds.mp3")

  Nostrum.Voice.pause(123456789)
  ```
  """
  @spec pause(Guild.id()) :: :noop | :ok
  def pause(guild_id) do
    voice = get_guild(guild_id)
    if VoiceState.playing?(voice) do
      set_speaking(voice, false)
      Process.exit(voice.player_pid, :pause)
      :ok
    else
      :noop
    end
  end

  @doc """
  Resume the current sound being played in a voice channel.

  The bot must be connected to a voice channel in the guild specified.

  ## Parameters
    - `guild_id` - ID of guild whose voice channel the sound will be resumed in.

  Returns `:noop` if unable to resume or no sound has been paused, otherwise returns `:ok`

  This function is used to resume a sound that had previously been paused.

  ```Elixir
  Nostrum.Voice.join_channel(123456789, 420691337)

  Nostrum.Voice.play(123456789, "~/stuff/Toto - Africa (Bass Boosted)")

  Nostrum.Voice.pause(123456789)

  Nostrum.Voice.resume(123456789)
  ```
  """
  @spec resume(Guild.id()) :: :noop | :ok
  def resume(guild_id) do
    voice = get_guild(guild_id)
    if VoiceState.playing?(voice) or is_nil(voice.ffmpeg_proc) do
      :noop
    else
      set_speaking(voice, true)
      {:ok, pid} = Task.start(fn -> Audio.player_loop(voice) end)
      update_guild(guild_id, player_pid: pid)
      :ok
    end
  end

  @doc false
  def set_speaking(%VoiceState{} = voice, speaking) do
    send(voice.session_pid, {:speaking, speaking})
  end

  @doc false
  def set_speaking(guild, speaking) do
    get_guild(guild) |> set_speaking(speaking)
  end

  @doc false
  def handle_call({:update, guild, args}, _from, state) do
    voice =
      state
      |> Map.get(guild, VoiceState.new(guild: guild))
      |> Map.merge(Enum.into(args, %{}))

    state = Map.put(state, guild, voice)
    start_if_ready(voice)
    {:reply, voice, state}
  end

  @doc false
  def handle_call({:get, guild}, _from, state) do
    {:reply, Map.get(state, guild), state}
  end

  @doc false
  def handle_cast({:remove, guild}, state) do
    VoiceSupervisor.end_session(guild)
    Map.get(state, guild) |> VoiceState.cleanup()

    {:noreply, Map.delete(state, guild)}
  end

  @doc false
  def start_if_ready(%VoiceState{} = voice) do
    if VoiceState.ready_for_ws?(voice) do
      VoiceSupervisor.create_session(voice)
    end
  end
end
