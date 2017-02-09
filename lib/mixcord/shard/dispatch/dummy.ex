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

defmodule Mixcord.Producer.Events do
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
  @type state :: Map.t

  @type channel_create :: {:CHANNEL_CREATE, Mixcord.Struct.Channel.t}
  @type channel_delete :: {:CHANNEL_DELETE, Mixcord.Struct.Channel.t}
  @type channel_update :: {:CHANNEL_UPDATE, old_channel :: Mixcord.Struct.Channel.t, new_channel :: Mixcord.Struct.Channel.t}
  @type channel_pins_ack :: {:CHANNEL_PINS_ACK, Map.t}
  @type channel_pins_update :: {:CHANNEL_PINS_UPDATE, Map.t}
  @type guild_ban_add :: {:GUILD_BAN_ADD, Mixcord.Struct.User.t}
  @type build_ban_remove :: {:GUILD_BAN_REMOVE, Mixcord.Struct.User.t}
  @type guild_create :: {:GUILD_CREATE, new_guild :: Mixcord.Struct.Guild.t}
  @type guild_update :: {:GUILD_CREATE, old_guild :: Mixcord.Struct.Guild.t, new_guild :: Mixcord.Struct.Guild.t}
  @type guild_delete :: {:GUILD_DELETE, old_guild :: Mixcord.Struct.Guild.t}
  @type guild_emojis_update :: {:GUILD_EMOJIS_UPDATE, old_emojis :: [Mixcord.Struct.Emoji],  new_emojis :: [Mixcord.Struct.Emoji]}
  @type guild_integrations_update :: {:GUILD_INTEGERATIONS_UPDATE, Map.t}
  @type guild_member_add :: {:GUILD_MEMBER_ADD, new_member :: Mixcord.Struct.Member.t}
  @type guild_members_chunk :: {:GUILD_MEMBERS_CHUNK, Map.t}
  @type guild_member_remove :: {:GUILD_MEMBER_REMOVE, old_member :: Mixcord.Struct.Member.t}
  @type guild_member_update :: {:GUILD_MEMBER_UPDATE, old_member :: Mixcord.Struct.Member.t, new_member :: Mixcord.Struct.Member.t}
  @type guild_role_create :: {:GUILD_ROLE_CREATE, new_role :: Mixcord.Struct.Role.t}
  @type guild_role_delete :: {:GUILD_ROLE_DELETE, old_role :: Mixcord.Struct.Role.t}
  @type guild_role_update :: {:GUILD_ROLE_UPDATE, old_role :: Mixcord.Struct.Role.t, new_role :: Mixcord.Struct.Role.t}
  @type message_create :: {:MESSAGE_CREATE, message :: Mixcord.Struct.Message.t}
  @type message_delete :: {:MESSAGE_DELETE, message :: Mixcord.Struct.Message.t}
  @type message_delete_bulk :: {:MESSAGE_DELETE_BULK, updated_messages :: [Mixcord.Struct.Message.t]}
  @type message_update :: {:MESSAGE_UPDATE, updated_message :: Mixcord.Struct.Message.t}
  @type presence_update :: {:PRESENCE_UPDATE, Map.t}
  @type ready :: {:READY, Map.t}
  @type resumed :: {:RESUMED, Map.t}
  @type typing_start :: {:TYPING_START, Map.t}
  @type user_settings_update :: no_return
  @type user_update :: {:USER_UPDATE, old_user :: Mixcord.Struct.User.t, new_user :: Mixcord.Struct.User.t}
  @type voice_state_update :: {:VOICE_STATE_UPDATE, Map.t}
  @type voice_server_update :: {:VOICE_SERVER_UPDATE, Map.t}

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
