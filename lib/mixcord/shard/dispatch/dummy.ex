defmodule Dummy do
  def start do
    import Supervisor.Spec

    children = [
      worker(DummyConsumer, [], id: 1),
      worker(DummyConsumer, [], id: 2),
      worker(DummyConsumer, [], id: 3),
      worker(DummyConsumer, [], id: 4)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule Mixcord.Shard.Dispatch.Consumer.Events do
  @moduledoc """
  Defines the events you can handle in a consumer.
  """

  @typedoc """
  Tuple describing the client of a call request.

  `pid` is the PID of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @type ws_state :: state
  @type state :: term

  @type channel_create :: {:CHANNEL_CREATE, Mixcord.Struct.Channel.t}
  @type channel_delete :: {:CHANNEL_DELETE, Mixcord.Struct.Channel.t}
  @type channel_update :: {:CHANNEL_UPDATE, old_channel :: Mixcord.Struct.Channel.t, new_channel :: Mixcord.Struct.Channel.t}
  @type channel_pins_ack :: no_return
  @type channel_pins_update :: no_return
  @type guild_ban_add :: no_return
  @type build_ban_remove :: no_return
  @type guild_create :: {:GUILD_CREATE, old_guild :: Mixcord.Struct.Guild.t}
  @type guild_update :: no_return
  @type guild_delete :: no_return
  @type guild_emojis_update :: no_return
  @type guild_integrations_update ::no_return
  @type guild_member_add :: no_return
  @type guild_members_chunk :: no_return
  @type guild_member_remove :: no_return
  @type guild_member_update :: no_return
  @type guild_role_create :: no_return
  @type guild_role_delete :: no_return
  @type guild_role_update :: no_return
  @type message_create :: no_return
  @type message_delete :: no_return
  @type message_delete_bulk :: no_return
  @type message_update :: no_return
  @type presence_update :: no_return
  @type ready :: no_return
  @type resumed :: no_return
  @type typing_start :: no_return
  @type user_settings_update :: no_return
  @type user_update :: no_return
  @type voice_state_update :: no_return
  @type voice_server_update :: no_return

  @type event :: channel_create |
    channel_delete |
    channel_update |
    channel_pins_ack |
    channel_pins_update |
    guild_ban_add |
    build_ban_remove |
    guild_create |
    guild_update |
    guild_delete |
    guild_emojis_update |
    guild_integrations_update |
    guild_member_add |
    guild_members_chunk |
    guild_member_remove |
    guild_member_update |
    guild_role_create |
    guild_role_delete |
    guild_role_update |
    message_create |
    message_delete |
    message_delete_bulk |
    message_update |
    presence_update |
    ready |
    resumed |
    typing_start |
    user_settings_update |
    user_update |
    voice_state_update |
    voice_server_update

  @callback handle_event({event, ws_state}, from, state) :: {:noreply, [], state}
end

defmodule DummyConsumer do

  use GenStage
  require Logger

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    {:consumer, :ok, subscribe_to: [Mixcord.Shard.Dispatch.Producer]}
  end

  def handle_events(events, _from, state) do
    for event <- events do
      {{event_name, _payload}, _state} = event
      Logger.debug "User would process event #{event_name} here on pid #{inspect self()}"
    end
    {:noreply, [], state}
  end
end