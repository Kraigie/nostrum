defmodule Nostrum.Consumer do
  @moduledoc """
  Consumer process for gateway event handling.

  # Consuming Gateway Events

  Events are first ingested by Nostrum's cache. Afterwards, they are sent to
  any subscribed consumers via `Nostrum.ConsumerGroup`.

  By default, nostrum will start a process for each event. This gives us free
  parallelism and isolation. You therefore do not need to start more than one
  consumer in your supervision tree.

  ## Example
  An example consumer can be found
  [here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer.ex).
  """

  alias Nostrum.ConsumerGroup

  alias Nostrum.Struct.{
    AutoModerationRule,
    Channel,
    Interaction,
    ThreadMember,
    VoiceWSState,
    WSState
  }

  alias Nostrum.Struct.Guild.{
    AuditLogEntry,
    Integration
  }

  alias Nostrum.Struct.Event.{
    AutoModerationRuleExecute,
    ChannelPinsUpdate,
    GuildBanAdd,
    GuildBanRemove,
    GuildIntegrationDelete,
    GuildIntegrationsUpdate,
    GuildScheduledEventUserAdd,
    GuildScheduledEventUserRemove,
    MessageDelete,
    MessageDeleteBulk,
    MessageReactionAdd,
    MessageReactionRemove,
    MessageReactionRemoveAll,
    MessageReactionRemoveEmoji,
    Ready,
    SpeakingUpdate,
    ThreadListSync,
    ThreadMembersUpdate,
    TypingStart,
    VoiceReady,
    VoiceServerUpdate,
    VoiceState
  }

  @doc """
  Callback used to handle events.

  ## Event
  `event` is a tuple describing the event. The tuple will include information in
  the following format:
  ```elixir
  {event_name, {event_payload(s)}, WSState.t}
  ```

  For example, a message create will look like this
  ```elixir
  {:MESSAGE_CREATE, Nostrum.Struct.Message.t, WSState.t}
  ```

  In some cases there will be multiple payloads when something is updated, so as
  to include the new and the old versions. In the event of there being two payloads,
  the old payload will always be first, followed by the new payload.
  ```elixir
  {:USER_UPDATE, {old_user :: Nostrum.Struct.User.t, new_user :: Nostrum.Struct.User.t}, WSState.t()}
  ```

  For a full listing of events, please see `t:Nostrum.Consumer.event/0`.
  """
  @callback handle_event(event) :: any

  @type auto_moderation_rule_create ::
          {:AUTO_MODERATION_RULE_CREATE, AutoModerationRule.t(), WSState.t()}
  @type auto_moderation_rule_delete ::
          {:AUTO_MODERATION_RULE_DELETE, AutoModerationRule.t(), WSState.t()}
  @type auto_moderation_rule_update ::
          {:AUTO_MODERATION_RULE_UPDATE, AutoModerationRule.t(), WSState.t()}

  @type auto_moderation_rule_execute ::
          {:AUTO_MODERATION_RULE_EXECUTE, AutoModerationRuleExecute.t(), WSState.t()}

  @typedoc """
  Dispatched when a channel is created.

  Starting from [API and Gateway V8](https://discord.com/developers/docs/change-log#api-and-gateway-v8),
  this will never be sent for a DM.
  """
  @type channel_create :: {:CHANNEL_CREATE, Channel.t(), WSState.t()}
  @type channel_delete :: {:CHANNEL_DELETE, Channel.t(), WSState.t()}
  @typedoc """
  Dispatched when a channel is updated.

  `old_channel` will be `nil` when the pre-update channel could not be fetched from the cache.
  """
  @type channel_update ::
          {:CHANNEL_UPDATE, {old_channel :: Channel.t() | nil, new_channel :: Channel.t()},
           WSState.t()}
  @type channel_pins_ack :: {:CHANNEL_PINS_ACK, map, WSState.t()}
  @type channel_pins_update :: {:CHANNEL_PINS_UPDATE, ChannelPinsUpdate.t(), WSState.t()}
  @type guild_audit_log_entry_create ::
          {:GUILD_AUDIT_LOG_ENTRY_CREATE, AuditLogEntry.t(), WSState.t()}
  @type guild_ban_add ::
          {:GUILD_BAN_ADD, GuildBanAdd.t(), WSState.t()}
  @type guild_ban_remove ::
          {:GUILD_BAN_REMOVE, GuildBanRemove.t(), WSState.t()}
  @type guild_create :: {:GUILD_CREATE, new_guild :: Nostrum.Struct.Guild.t(), WSState.t()}
  @type guild_available :: {:GUILD_AVAILABLE, new_guild :: Nostrum.Struct.Guild.t(), WSState.t()}
  @type guild_unavailable ::
          {:GUILD_UNAVAILABLE, unavailable_guild :: Nostrum.Struct.Guild.UnavailableGuild.t(),
           WSState.t()}
  @type guild_update ::
          {:GUILD_UPDATE,
           {old_guild :: Nostrum.Struct.Guild.t(), new_guild :: Nostrum.Struct.Guild.t()},
           WSState.t()}
  @type guild_delete ::
          {:GUILD_DELETE, {old_guild :: Nostrum.Struct.Guild.t(), unavailable :: boolean},
           WSState.t()}
  @type guild_emojis_update ::
          {:GUILD_EMOJIS_UPDATE,
           {guild_id :: integer, old_emojis :: [Nostrum.Struct.Emoji.t()],
            new_emojis :: [Nostrum.Struct.Emoji.t()]}, WSState.t()}
  @type guild_integrations_update ::
          {:GUILD_INTEGRATIONS_UPDATE, GuildIntegrationsUpdate.t(), WSState.t()}
  @type guild_member_add ::
          {:GUILD_MEMBER_ADD,
           {guild_id :: integer, new_member :: Nostrum.Struct.Guild.Member.t()}, WSState.t()}
  @type guild_members_chunk :: {:GUILD_MEMBERS_CHUNK, map, WSState.t()}
  @typedoc """
  Dispatched when somebody leaves a guild.

  In case the guild member intent is enabled but not the guild intent,
  nostrum may not cache the actual guild, and thus be unable to provide
  full information about members leaving guilds. In that case, this event
  receives the guild ID and a partial member object with the leaving user as
  provided by Discord, but no information about the user's state on the guild.
  """
  @type guild_member_remove ::
          {:GUILD_MEMBER_REMOVE,
           {guild_id :: integer, old_member :: Nostrum.Struct.Guild.Member.t()}, WSState.t()}
  @typedoc """
  Dispatched when a guild member is updated.

  `old_member` will be `nil` when the pre-update member could not be fetched from the cache.
  """
  @type guild_member_update ::
          {:GUILD_MEMBER_UPDATE,
           {guild_id :: integer, old_member :: Nostrum.Struct.Guild.Member.t() | nil,
            new_member :: Nostrum.Struct.Guild.Member.t()}, WSState.t()}
  @type guild_role_create ::
          {:GUILD_ROLE_CREATE, {guild_id :: integer, new_role :: Nostrum.Struct.Guild.Role.t()},
           WSState.t()}
  @type guild_role_delete ::
          {:GUILD_ROLE_DELETE, {guild_id :: integer, old_role :: Nostrum.Struct.Guild.Role.t()},
           WSState.t()}
  @typedoc """
  Dispatched when a role on a guild is updated.

  `old_role` will be `nil` when the pre-update role could not be fetched from the cache.
  """
  @type guild_role_update ::
          {:GUILD_ROLE_UPDATE,
           {guild_id :: integer, old_role :: Nostrum.Struct.Guild.Role.t() | nil,
            new_role :: Nostrum.Struct.Guild.Role.t()}, WSState.t()}
  @type guild_scheduled_event_create ::
          {:GUILD_SCHEDULED_EVENT_CREATE, Nostrum.Struct.Guild.ScheduledEvent.t(), WSState.t()}
  @type guild_scheduled_event_delete ::
          {:GUILD_SCHEDULED_EVENT_DELETE, Nostrum.Struct.Guild.ScheduledEvent.t(), WSState.t()}
  @type guild_scheduled_event_update ::
          {:GUILD_SCHEDULED_EVENT_UPDATE, Nostrum.Struct.Guild.ScheduledEvent.t(), WSState.t()}
  @type guild_scheduled_event_user_add ::
          {:GUILD_SCHEDULED_EVENT_USER_ADD, GuildScheduledEventUserAdd.t(), WSState.t()}
  @type guild_scheduled_event_user_remove ::
          {:GUILD_SCHEDULED_EVENT_USER_REMOVE, GuildScheduledEventUserRemove.t(), WSState.t()}

  @typedoc since: "0.5.1"
  @type integration_create :: {:INTEGRATION_CREATE, Integration.t(), WSState.t()}

  @typedoc """
  Different from `t:guild_integrations_update/0` in that more than only the `guild_id` is provided
  """
  @typedoc since: "0.5.1"
  @type integration_update :: {:INTEGRATION_UPDATE, Integration.t(), WSState.t()}
  @typedoc since: "0.5.1"
  @type integration_delete :: {:INTEGRATION_DELETE, GuildIntegrationDelete.t(), WSState.t()}
  @type interaction_create :: {:INTERACTION_CREATE, Interaction.t(), WSState.t()}
  @type message_create :: {:MESSAGE_CREATE, message :: Nostrum.Struct.Message.t(), WSState.t()}
  @type message_delete :: {:MESSAGE_DELETE, MessageDelete.t(), WSState.t()}
  @type message_delete_bulk :: {:MESSAGE_DELETE_BULK, MessageDeleteBulk.t(), WSState.t()}
  @type message_update ::
          {:MESSAGE_UPDATE, updated_message :: Nostrum.Struct.Message.t(), WSState.t()}
  @type message_reaction_add :: {:MESSAGE_REACTION_ADD, MessageReactionAdd.t(), WSState.t()}
  @type message_reaction_remove ::
          {:MESSAGE_REACTION_REMOVE, MessageReactionRemove.t(), WSState.t()}
  @type message_reaction_remove_all ::
          {:MESSAGE_REACTION_REMOVE_ALL, MessageReactionRemoveAll.t(), WSState.t()}
  @type message_reaction_remove_emoji ::
          {:MESSAGE_REACTION_REMOVE_EMOJI, MessageReactionRemoveEmoji.t(), WSState.t()}
  @type message_ack :: {:MESSAGE_ACK, map, WSState.t()}
  @typedoc """
  Dispatched when a user's presence is updated.

  `old_presence` will be `nil` when the pre-update presence could not be fetched from the cache.
  """
  @type presence_update ::
          {:PRESENCE_UPDATE,
           {guild_id :: integer, old_presence :: map | nil, new_presence :: map}, WSState.t()}
  @type ready :: {:READY, Ready.t(), WSState.t()}
  @type resumed :: {:RESUMED, map, WSState.t()}
  @type typing_start :: {:TYPING_START, TypingStart.t(), WSState.t()}
  @type user_settings_update :: no_return
  @typedoc """
  Dispatched when a user is updated.

  `old_user` will be `nil` when the pre-update user could not be fetched from the cache.
  """
  @type user_update ::
          {:USER_UPDATE,
           {old_user :: Nostrum.Struct.User.t() | nil, new_user :: Nostrum.Struct.User.t()},
           WSState.t()}
  @typedoc """
  Dispatched when the bot is ready to begin sending audio after joining a voice channel

  Note that the third tuple element is of type `t:Nostrum.Struct.VoiceWSState.t/0` instead of `t:Nostrum.Struct.WSState.t/0`.
  """
  @typedoc since: "0.5.0"
  @type voice_ready :: {:VOICE_READY, VoiceReady.t(), VoiceWSState.t()}
  @typedoc """
  Dispatched when the bot starts or stops speaking

  Note that the third tuple element is of type `t:Nostrum.Struct.VoiceWSState.t/0` instead of `t:Nostrum.Struct.WSState.t/0`.
  """
  @type voice_speaking_update :: {:VOICE_SPEAKING_UPDATE, SpeakingUpdate.t(), VoiceWSState.t()}
  @typedoc """
  Dispatched when async listening is enabled and another user is actively speaking

  The second tuple element is an `t:Nostrum.Voice.rtp_opus/0`, which is a tuple with
  RTP header information and an opus packet. While someone is actively talking, you can
  expect about 50 events per second per speaking user.

  Note that the third tuple element is of type `t:Nostrum.Struct.VoiceWSState.t/0` instead of `t:Nostrum.Struct.WSState.t/0`.
  That struct contains a `t:Nostrum.Struct.VoiceWSState.ssrc_map/0` that can determine the speaking user based
  on the SSRC.
  """
  @typedoc since: "0.6.0"
  @type voice_incoming_packet ::
          {:VOICE_INCOMING_PACKET, Nostrum.Voice.rtp_opus(), VoiceWSState.t()}
  @type voice_state_update :: {:VOICE_STATE_UPDATE, VoiceState.t(), WSState.t()}
  @type voice_server_update :: {:VOICE_SERVER_UPDATE, VoiceServerUpdate.t(), WSState.t()}
  @type webhooks_update :: {:WEBHOOKS_UPDATE, map, WSState.t()}

  @typedoc """
  Dispatched when a thread is created or when added to a private thread
  """
  @type thread_create :: {:THREAD_CREATE, Channel.t(), WSState.t()}

  @typedoc """
  Dispatched when a thread is deleted, if the thread was cached, contains the original thread, otherwise contains `:noop`
  """
  @type thread_delete :: {:THREAD_DELETE, Channel.t() | :noop, WSState.t()}
  @type thread_update ::
          {:THREAD_UPDATE, {old_thread :: Channel.t() | nil, new_thread :: Channel.t()},
           WSState.t()}

  @typedoc """
  Dispatched when gaining access to a channel
  """
  @type thread_list_sync :: {:THREAD_LIST_SYNC, ThreadListSync.t(), WSState.t()}

  @typedoc """
  Dispatched when a `ThreadMember` for the current user is updated
  """
  @type thread_member_update :: {:THREAD_MEMBER_UPDATE, ThreadMember.t(), WSState.t()}

  @typedoc """
  Dispatched when member(s) are added or removed from a thread
  """
  @type thread_members_update :: {:THREAD_MEMBERS_UPDATE, ThreadMembersUpdate.t(), WSState.t()}
  @type event ::
          auto_moderation_rule_create
          | auto_moderation_rule_delete
          | auto_moderation_rule_update
          | auto_moderation_rule_execute
          | channel_create
          | channel_delete
          | channel_update
          | channel_pins_ack
          | channel_pins_update
          | guild_audit_log_entry_create
          | guild_ban_add
          | guild_ban_remove
          | guild_create
          | guild_available
          | guild_unavailable
          | guild_update
          | guild_delete
          | guild_emojis_update
          | guild_integrations_update
          | guild_member_add
          | guild_members_chunk
          | guild_member_remove
          | guild_member_update
          | guild_role_create
          | guild_role_delete
          | guild_role_update
          | integration_create
          | integration_delete
          | integration_update
          | interaction_create
          | message_create
          | message_delete
          | message_delete_bulk
          | message_update
          | message_reaction_add
          | message_reaction_remove
          | message_reaction_remove_all
          | message_ack
          | presence_update
          | ready
          | resumed
          | thread_create
          | thread_delete
          | thread_update
          | thread_list_sync
          | thread_member_update
          | thread_members_update
          | typing_start
          | user_settings_update
          | user_update
          | voice_ready
          | voice_speaking_update
          | voice_incoming_packet
          | voice_state_update
          | voice_server_update
          | webhooks_update

  defmacro __using__(_opts) do
    quote location: :keep do
      use GenServer

      def start_link(opts) do
        GenServer.start_link(__MODULE__, [], opts)
      end

      def init([]) do
        {:ok, nil, {:continue, nil}}
      end

      def handle_continue(_args, state) do
        ConsumerGroup.join(self())
        {:noreply, state}
      end

      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :worker,
          restart: :permanent,
          max_restarts: 0,
          shutdown: 500
        }
      end

      def handle_info({:event, event}, state) do
        Task.start_link(fn ->
          __MODULE__.handle_event(event)
        end)

        {:noreply, state}
      end
    end
  end
end
