defmodule Nostrum.Shard.Dispatch do
  @moduledoc false

  alias Nostrum.Cache.{
    ChannelGuildMapping,
    GuildCache,
    MemberCache,
    MessageCache,
    PresenceCache,
    UserCache
  }

  alias Nostrum.Cache.Me
  alias Nostrum.Shard.{Intents, Session}
  alias Nostrum.Store.GuildShardMapping
  alias Nostrum.Store.UnavailableGuild, as: UnavailableGuildStore

  alias Nostrum.Struct.Event.{
    AutoModerationRuleExecute,
    ChannelPinsUpdate,
    GuildBanAdd,
    GuildBanRemove,
    GuildIntegrationDelete,
    GuildIntegrationsUpdate,
    GuildScheduledEventUserAdd,
    GuildScheduledEventUserRemove,
    InviteCreate,
    InviteDelete,
    MessageDelete,
    MessageDeleteBulk,
    MessageReactionAdd,
    MessageReactionRemove,
    MessageReactionRemoveAll,
    MessageReactionRemoveEmoji,
    PollVoteChange,
    Ready,
    SpeakingUpdate,
    ThreadListSync,
    ThreadMembersUpdate,
    TypingStart,
    VoiceReady,
    VoiceServerUpdate,
    VoiceState
  }

  alias Nostrum.Struct.{AutoModerationRule, Interaction, ThreadMember, User}
  alias Nostrum.Struct.Guild.{AuditLogEntry, Integration, ScheduledEvent, UnavailableGuild}
  alias Nostrum.Util
  alias Nostrum.Voice

  require Logger

  @large_threshold 250

  def handle({payload, state}) do
    if Application.get_env(:nostrum, :log_full_events),
      do: Logger.debug(inspect(payload.d, pretty: true))

    payload.t
    |> handle_event(payload.d, state)
    |> format_event
  end

  defp format_event(events) when is_list(events),
    do: for(event <- events, do: format_event(event))

  # Handles the case of not finding users in the user cache
  defp format_event({_name, :noop, _state}), do: :noop
  defp format_event({_name, event_info, _state} = event) when is_tuple(event_info), do: event
  defp format_event({name, event_info, state}), do: {name, event_info, state}
  defp format_event(:noop), do: :noop

  defp check_new_or_unavailable(guild_id) do
    if UnavailableGuildStore.is?(guild_id) do
      :GUILD_AVAILABLE
    else
      :GUILD_CREATE
    end
  end

  def handle_event(:AUTO_MODERATION_RULE_CREATE = event, p, state),
    do: {event, AutoModerationRule.to_struct(p), state}

  def handle_event(:AUTO_MODERATION_RULE_DELETE = event, p, state),
    do: {event, AutoModerationRule.to_struct(p), state}

  def handle_event(:AUTO_MODERATION_RULE_UPDATE = event, p, state),
    do: {event, AutoModerationRule.to_struct(p), state}

  def handle_event(:AUTO_MODERATION_RULE_EXECUTION = event, p, state),
    do: {event, AutoModerationRuleExecute.to_struct(p), state}

  def handle_event(:CHANNEL_CREATE = event, p, state) do
    ChannelGuildMapping.create(p.id, p.guild_id)
    {event, GuildCache.channel_create(p.guild_id, p), state}
  end

  def handle_event(:CHANNEL_DELETE = event, p, state) do
    channel_id = p.id

    # Starting this as a task to avoid blocking the dispatch process
    # since this is a potentially long operation
    Task.start(fn -> MessageCache.channel_delete(channel_id) end)

    ChannelGuildMapping.delete(channel_id)
    {event, GuildCache.channel_delete(p.guild_id, p.id), state}
  end

  def handle_event(:CHANNEL_UPDATE = event, p, state) do
    {event, GuildCache.channel_update(p.guild_id, p), state}
  end

  def handle_event(:CHANNEL_PINS_ACK = event, p, state), do: {event, p, state}

  def handle_event(:CHANNEL_PINS_UPDATE = event, p, state) do
    {event, ChannelPinsUpdate.to_struct(p), state}
  end

  def handle_event(:GUILD_AUDIT_LOG_ENTRY_CREATE = event, p, state),
    do: {event, AuditLogEntry.to_struct(p), state}

  def handle_event(:GUILD_BAN_ADD = event, p, state) do
    {event, GuildBanAdd.to_struct(p), state}
  end

  def handle_event(:GUILD_BAN_REMOVE = event, p, state) do
    {event, GuildBanRemove.to_struct(p), state}
  end

  def handle_event(:GUILD_CREATE, %{unavailable: true} = guild, state) do
    UnavailableGuildStore.create(guild.id)
    {:GUILD_UNAVAILABLE, UnavailableGuild.to_struct(guild), state}
  end

  def handle_event(:GUILD_CREATE, p, state) do
    # Ensures every channel will have an associated guild_id
    channels_with_guild_id =
      p.channels
      |> Enum.map(fn channel -> Map.put(channel, :guild_id, p.id) end)

    guild = %{p | channels: channels_with_guild_id}

    Enum.each(guild.members, fn member -> UserCache.create(member.user) end)
    MemberCache.bulk_create(guild.id, guild.members)

    GuildShardMapping.create(guild.id, state.shard_num)

    Enum.each(guild.channels, fn channel ->
      ChannelGuildMapping.create(channel.id, guild.id)
    end)

    has_members = Intents.has_intent?(:guild_members)
    has_presences = Intents.has_intent?(:guild_presences)

    intents_should_request? = has_members and not has_presences
    large_server? = guild.member_count >= @large_threshold

    should_request? = large_server? or intents_should_request?

    if should_request? and Application.get_env(:nostrum, :request_guild_members, false) do
      Session.request_guild_members(state.conn_pid, guild.id)
    end

    {presences, guild} = Map.pop(guild, :presences, [])
    PresenceCache.bulk_create(guild.id, presences)

    casted = GuildCache.create(guild)
    {check_new_or_unavailable(casted.id), casted, state}
  end

  def handle_event(:GUILD_UPDATE = event, p, state), do: {event, GuildCache.update(p), state}

  def handle_event(:GUILD_DELETE = event, p, state) do
    GuildShardMapping.delete(p.id)
    {event, {GuildCache.delete(p.id), Map.get(p, :unavailable, false)}, state}
  end

  def handle_event(:GUILD_EMOJIS_UPDATE = event, p, state),
    do: {event, GuildCache.emoji_update(p.guild_id, p.emojis), state}

  def handle_event(:GUILD_STICKERS_UPDATE = event, p, state),
    do: {event, GuildCache.stickers_update(p.guild_id, p.stickers), state}

  def handle_event(:GUILD_INTEGRATIONS_UPDATE = event, p, state) do
    {event, GuildIntegrationsUpdate.to_struct(p), state}
  end

  def handle_event(:GUILD_MEMBER_ADD = event, p, state) do
    GuildCache.member_count_up(p.guild_id)
    _new_user = UserCache.create(p.user)
    {event, {p.guild_id, MemberCache.create(p.guild_id, p)}, state}
  end

  def handle_event(:GUILD_MEMBERS_CHUNK = event, p, state) do
    Stream.map(p.members, & &1.user)
    |> UserCache.bulk_create()

    MemberCache.bulk_create(p.guild_id, p.members)

    # note: not casted at the moment, deemed mostly internal
    {event, p, state}
  end

  def handle_event(:GUILD_MEMBER_REMOVE = event, p, state) do
    GuildCache.member_count_down(p.guild_id)
    {event, MemberCache.delete(p.guild_id, p.user.id), state}
  end

  def handle_event(:GUILD_MEMBER_UPDATE = event, %{guild_id: guild_id} = p, state) do
    {event, MemberCache.update(guild_id, p), state}
  end

  def handle_event(:GUILD_ROLE_CREATE = event, p, state),
    do: {event, GuildCache.role_create(p.guild_id, p.role), state}

  def handle_event(:GUILD_ROLE_DELETE = event, p, state),
    do: {event, GuildCache.role_delete(p.guild_id, p.role_id), state}

  def handle_event(:GUILD_ROLE_UPDATE = event, %{guild_id: guild_id} = p, state),
    do: {event, GuildCache.role_update(guild_id, p.role), state}

  def handle_event(:GUILD_SCHEDULED_EVENT_CREATE = event, p, state),
    do: {event, ScheduledEvent.to_struct(p), state}

  def handle_event(:GUILD_SCHEDULED_EVENT_UPDATE = event, p, state),
    do: {event, ScheduledEvent.to_struct(p), state}

  def handle_event(:GUILD_SCHEDULED_EVENT_DELETE = event, p, state),
    do: {event, ScheduledEvent.to_struct(p), state}

  def handle_event(:GUILD_SCHEDULED_EVENT_USER_ADD = event, p, state),
    do: {event, GuildScheduledEventUserAdd.to_struct(p), state}

  def handle_event(:GUILD_SCHEDULED_EVENT_USER_REMOVE = event, p, state),
    do: {event, GuildScheduledEventUserRemove.to_struct(p), state}

  def handle_event(:INTEGRATION_CREATE = event, p, state),
    do: {event, Integration.to_struct(p), state}

  def handle_event(:INTEGRATION_DELETE = event, p, state),
    do: {event, GuildIntegrationDelete.to_struct(p), state}

  def handle_event(:INTEGRATION_UPDATE = event, p, state),
    do: {event, Integration.to_struct(p), state}

  def handle_event(:INVITE_CREATE = event, p, state),
    do: {event, InviteCreate.to_struct(p), state}

  def handle_event(:INVITE_DELETE = event, p, state),
    do: {event, InviteDelete.to_struct(p), state}

  def handle_event(:MESSAGE_CREATE = event, p, state),
    do: {event, MessageCache.create(p), state}

  def handle_event(:MESSAGE_DELETE = event, p, state) do
    deleted_message = MessageCache.delete(p.channel_id, p.id)
    {event, MessageDelete.to_struct(p, deleted_message), state}
  end

  def handle_event(:MESSAGE_DELETE_BULK = event, p, state) do
    deleted_messages = MessageCache.bulk_delete(p.channel_id, p.ids)
    p = Map.put(p, :deleted_messages, deleted_messages)
    {event, MessageDeleteBulk.to_struct(p), state}
  end

  def handle_event(:MESSAGE_UPDATE = event, p, state) do
    {event, MessageCache.update(p), state}
  end

  def handle_event(:MESSAGE_REACTION_ADD = event, p, state) do
    {event, MessageReactionAdd.to_struct(p), state}
  end

  def handle_event(:MESSAGE_REACTION_REMOVE = event, p, state) do
    {event, MessageReactionRemove.to_struct(p), state}
  end

  def handle_event(:MESSAGE_REACTION_REMOVE_ALL = event, p, state) do
    {event, MessageReactionRemoveAll.to_struct(p), state}
  end

  def handle_event(:MESSAGE_REACTION_REMOVE_EMOJI = event, p, state) do
    {event, MessageReactionRemoveEmoji.to_struct(p), state}
  end

  def handle_event(:MESSAGE_ACK = event, p, state), do: {event, p, state}

  def handle_event(:PRESENCE_UPDATE = event, p, state) do
    [
      {event, PresenceCache.update(p), state}
      | [handle_event(:USER_UPDATE, p.user, state)]
    ]
  end

  def handle_event(:READY = event, p, state) do
    ready_guilds =
      p.guilds
      |> Enum.map(fn guild -> handle_event(:GUILD_CREATE, guild, state) end)

    current_user = Util.cast(p.user, {:struct, User})
    Me.put(current_user)

    [{event, Ready.to_struct(p), state} | ready_guilds]
  end

  def handle_event(:RESUMED = event, p, state), do: {event, p, state}

  def handle_event(:THREAD_CREATE = event, p, state),
    do: {event, GuildCache.channel_create(p.guild_id, p), state}

  def handle_event(:THREAD_DELETE = event, p, state),
    do: {event, GuildCache.channel_delete(p.guild_id, p.id), state}

  def handle_event(:THREAD_UPDATE = event, p, state),
    do: {event, GuildCache.channel_update(p.guild_id, p), state}

  def handle_event(:THREAD_LIST_SYNC = event, p, state),
    do: {event, ThreadListSync.to_struct(p), state}

  def handle_event(:THREAD_MEMBER_UPDATE = event, p, state),
    do: {event, ThreadMember.to_struct(p), state}

  def handle_event(:THREAD_MEMBERS_UPDATE = event, p, state),
    do: {event, ThreadMembersUpdate.to_struct(p), state}

  def handle_event(:TYPING_START = event, p, state) do
    {event, TypingStart.to_struct(p), state}
  end

  def handle_event(:USER_SETTINGS_UPDATE = event, p, state), do: {event, p, state}

  def handle_event(:USER_UPDATE = event, p, state) do
    if Me.get().id === p.id do
      Me.update(p)
    end

    {event, UserCache.update(p), state}
  end

  def handle_event(:VOICE_READY = event, p, state) do
    voice = Voice.get_voice(p.guild_id)

    if voice.persist_playback,
      do: _result = Voice.resume(p.guild_id)

    {event, VoiceReady.to_struct(p), state}
  end

  def handle_event(:VOICE_SPEAKING_UPDATE = event, p, state),
    do: {event, SpeakingUpdate.to_struct(p), state}

  def handle_event(:VOICE_INCOMING_PACKET = event, p, state),
    do: {event, p, state}

  def handle_event(:VOICE_STATE_UPDATE = event, p, state) do
    if Me.get().id === p.user_id do
      if p.channel_id do
        # Joining Channel
        voice = Voice.get_voice(p.guild_id)

        # credo:disable-for-next-line Credo.Check.Refactor.Nesting
        cond do
          # Not yet in a channel:
          is_nil(voice) or is_nil(voice.session) ->
            Voice.on_channel_join_new(p)

          # Already in different channel:
          voice.channel_id != p.channel_id and is_pid(voice.session_pid) ->
            Voice.on_channel_join_change(p, voice)

          # Already in this channel but connection died:
          is_pid(voice.session_pid) and not Process.alive?(voice.session_pid) ->
            Voice.restart_session(p)

          # Already in this channel:
          true ->
            :noop
        end
      else
        # Leaving Channel:
        Voice.remove_voice(p.guild_id)
      end
    end

    {_updated, _states} = GuildCache.voice_state_update(p.guild_id, p)
    {event, VoiceState.to_struct(p), state}
  end

  def handle_event(:VOICE_SERVER_UPDATE = event, p, state) do
    Voice.update_voice(p.guild_id,
      token: p.token,
      gateway: p.endpoint
    )

    {event, VoiceServerUpdate.to_struct(p), state}
  end

  def handle_event(:WEBHOOKS_UPDATE = event, p, state), do: {event, p, state}

  def handle_event(:INTERACTION_CREATE = event, p, state) do
    {event, Interaction.to_struct(p), state}
  end

  def handle_event(:MESSAGE_POLL_VOTE_ADD = event, p, state) do
    {event, PollVoteChange.to_struct(Map.merge(p, %{type: :add})), state}
  end

  def handle_event(:MESSAGE_POLL_VOTE_REMOVE = event, p, state) do
    {event, PollVoteChange.to_struct(Map.merge(p, %{type: :remove})), state}
  end

  def handle_event(event, p, state) do
    Logger.warning("UNHANDLED GATEWAY DISPATCH EVENT TYPE: #{event}, #{inspect(p)}")
    {event, p, state}
  end
end
