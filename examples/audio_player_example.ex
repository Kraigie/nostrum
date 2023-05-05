# This file can be placed somewhere in ./lib, and it can be started
# by running iex -S mix then calling AudioPlayerSupervisor.start_link([]).
defmodule AudioPlayerSupervisor do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [AudioPlayerConsumer]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule AudioPlayerConsumer do
  use Nostrum.Consumer

  alias Nostrum.Api
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Voice

  require Logger

  # Soundcloud link will be fed through youtube-dl
  @soundcloud_url "https://soundcloud.com/fyre-brand/level-up"
  # Audio file will be fed directly to ffmpeg
  @nut_file_url "https://brandthill.com/files/nut.wav"

  # Compile-time helper for defining Discord Application Command options
  opt = fn type, name, desc, opts ->
    %{type: type, name: name, description: desc}
    |> Map.merge(Enum.into(opts, %{}))
  end

  @play_opts [
    opt.(1, "song", "Play a song", []),
    opt.(1, "nut", "Play a nut sound", []),
    opt.(1, "file", "Play a file", options: [opt.(3, "url", "File URL to play", required: true)]),
    opt.(1, "url", "Play a URL from a common service",
      options: [opt.(3, "url", "URL to play", required: true)]
    )
  ]

  @commands [
    {"summon", "Summon bot to your voice channel", []},
    {"leave", "Tell bot to leave your voice channel", []},
    {"play", "Play a sound", @play_opts},
    {"stop", "Stop the playing sound", []},
    {"pause", "Pause the playing sound", []},
    {"resume", "Resume the paused sound", []}
  ]

  def get_voice_channel_of_interaction(%{guild_id: guild_id, user: %{id: user_id}} = _interaction) do
    guild_id
    |> GuildCache.get!()
    |> Map.get(:voice_states)
    |> Enum.find(%{}, fn v -> v.user_id == user_id end)
    |> Map.get(:channel_id)
  end

  # If you are running this example in an iex session where you manually call
  # AudioPlayerSupervisor.start_link, you will have to call this function
  # with your guild_id as the argument
  def create_guild_commands(guild_id) do
    Enum.each(@commands, fn {name, description, options} ->
      Api.create_guild_application_command(guild_id, %{
        name: name,
        description: description,
        options: options
      })
    end)
  end

  def handle_event({:READY, %{guilds: guilds} = _event, _ws_state}) do
    guilds
    |> Enum.map(fn guild -> guild.id end)
    |> Enum.each(&create_guild_commands/1)
  end

  def handle_event({:INTERACTION_CREATE, interaction, _ws_state}) do
    # Run the command, and check for a response message, or default to a checkmark emoji
    message =
      case do_command(interaction) do
        {:msg, msg} -> msg
        _ -> ":white_check_mark:"
      end

    Api.create_interaction_response(interaction, %{type: 4, data: %{content: message}})
  end

  def handle_event({:VOICE_SPEAKING_UPDATE, payload, _ws_state}) do
    Logger.debug("VOICE SPEAKING UPDATE #{inspect(payload)}")
  end

  # Default event handler, if you don't include this, your consumer WILL crash if
  # you don't have a method definition for each event type.
  def handle_event(_event) do
    :noop
  end

  def do_command(%{guild_id: guild_id, data: %{name: "summon"}} = interaction) do
    case get_voice_channel_of_interaction(interaction) do
      nil ->
        {:msg, "You must be in a voice channel to summon me"}

      voice_channel_id ->
        Voice.join_channel(guild_id, voice_channel_id)
    end
  end

  def do_command(%{guild_id: guild_id, data: %{name: "leave"}}) do
    Voice.leave_channel(guild_id)
    {:msg, "See you later :wave:"}
  end

  def do_command(%{guild_id: guild_id, data: %{name: "pause"}}), do: Voice.pause(guild_id)

  def do_command(%{guild_id: guild_id, data: %{name: "resume"}}), do: Voice.resume(guild_id)

  def do_command(%{guild_id: guild_id, data: %{name: "stop"}}), do: Voice.stop(guild_id)

  def do_command(%{guild_id: guild_id, data: %{name: "play", options: options}}) do
    if Voice.ready?(guild_id) do
      case options do
        [%{name: "song"}] -> Voice.play(guild_id, @soundcloud_url, :ytdl)
        [%{name: "nut"}] -> Voice.play(guild_id, @nut_file_url, :url)
        [%{name: "file", options: [%{value: url}]}] -> Voice.play(guild_id, url, :url)
        [%{name: "url", options: [%{value: url}]}] -> Voice.play(guild_id, url, :ytdl)
      end
    else
      {:msg, "I must be in a voice channel before playing audio"}
    end
  end
end
