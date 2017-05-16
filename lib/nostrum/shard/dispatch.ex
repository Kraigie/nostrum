defmodule Nostrum.Shard.Dispatch do
  @moduledoc false

  alias Nostrum.Cache.{ChannelCache, UserCache}
  alias Nostrum.Cache.Guild.GuildServer
  alias Nostrum.Shard.Session
  alias Nostrum.Struct.Guild.UnavailableGuild

  require Logger

  @large_threshold 250

  def handle({payload, state}) do
    Logger.debug payload.t

    log? = Application.get_env(:nostrum, :log_full_events)
    if log?, do: Logger.info inspect payload.d, pretty: true

    handle_event(payload.t, payload.d, state)
  end

  def handle_event(:CHANNEL_CREATE = event, %{is_private: true} = p, state) do
    {event, ChannelCache.create(p), state}
  end

  def handle_event(:CHANNEL_CREATE = event, p, state) do
    :ets.insert(:channel_guild_map, {p.id, p.guild_id})
    {event, GuildServer.channel_create(p.guild_id, p), state}
  end

  def handle_event(:CHANNEL_DELETE = event, %{is_private: true} = p, state) do
    {event, ChannelCache.delete(p), state}
  end

  def handle_event(:CHANNEL_DELETE = event, p, state) do
    :ets.delete(:channel_guild_map, p.id)
    {event, GuildServer.channel_delete(p.guild_id, p.id), state}
  end

  def handle_event(:CHANNEL_UPDATE = event, p, state) do
    {event, GuildServer.channel_update(p.guild_id, p), state}
  end

  def handle_event(:CHANNEL_PINS_ACK = event, p, state),
    do: {event, p, state}

  def handle_event(:CHANNEL_PINS_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(:GUILD_BAN_ADD = event, p, state),
    do: {event, p, state}

  def handle_event(:BUILD_BAN_REMOVE = event, p, state),
    do: {event, p, state}

  def handle_event(:GUILD_CREATE, %{unavailable: true} = guild, state) do
    :ets.insert(:unavailable_guilds, {guild.id, guild})
    {:GUILD_UNAVAILABLE, UnavailableGuild.to_struct(guild), state}
  end

  def handle_event(:GUILD_CREATE, p, state) do
    p.members
    |> Enum.each(fn member -> UserCache.create(member.user) end)

    :ets.insert(:guild_shard_map, {p.id, state.shard_num})
    Enum.each(p.channels, fn channel ->
      :ets.insert(:channel_guild_map, {channel.id, p.id})
    end)

    if p.member_count >= @large_threshold do
      Session.request_guild_members(state.shard_pid, p.id)
    end

    case GuildServer.create(p) do
      {:error, reason} -> Logger.warn "Failed to create new guild process: #{inspect reason}"
      ok -> {check_new_or_unavailable(p.id), ok, state}
    end
  end

  defp check_new_or_unavailable(guild_id) do
    case :ets.lookup(:unavailable_guilds, guild_id) do
      [] -> :GUILD_CREATE
      [id] -> :GUILD_AVAILABLE
    end
  end

  def handle_event(:GUILD_UPDATE = event, p, state),
    do: {event, GuildServer.update(p), state}

  def handle_event(:GUILD_DELETE = event, p, state) do
    :ets.delete(:guild_shard_map, p.id)
    {event, GuildServer.delete(p.id), state}
  end

  def handle_event(:GUILD_EMOJIS_UPDATE = event, p, state),
    do: {event, GuildServer.emoji_update(p.guild_id, p.emojis), state}

  def handle_event(:GUILD_INTEGRATIONS_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(:GUILD_MEMBER_ADD = event, p, state) do
    UserCache.create(p.user)
    {event, GuildServer.member_add(p.guild_id, p), state}
  end

  def handle_event(:GUILD_MEMBERS_CHUNK = event, p, state) do
    p.members
    |> Enum.each(fn member ->
      UserCache.create(member.user)
      GuildServer.member_add(p.guild_id, member)
    end)
    {event, p, state}
  end

  def handle_event(:GUILD_MEMBER_REMOVE = event, p, state),
    do: {event, GuildServer.member_remove(p.guild_id, p.user), state}

  def handle_event(:GUILD_MEMBER_UPDATE = event, p, state),
    do: {event, GuildServer.member_update(p.guild_id, p), state}

  def handle_event(:GUILD_ROLE_CREATE = event, p, state),
    do: {event, GuildServer.role_create(p.guild_id, p.role), state}

  def handle_event(:GUILD_ROLE_DELETE = event, p, state),
    do: {event, GuildServer.role_delete(p.guild_id, p.role_id), state}

  def handle_event(:GUILD_ROLE_UPDATE = event, p, state),
    do: {event, GuildServer.role_update(p.guild_id, p.role), state}

  def handle_event(:MESSAGE_CREATE = event, p, state),
    do: {event, p, state}

  def handle_event(:MESSAGE_DELETE = event, p, state),
    do: {event, p, state}

  def handle_event(:MESSAGE_DELETE_BULK = event, p, state),
    do: {event, p, state}

  def handle_event(:MESSAGE_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(:MESSAGE_REACTION_ADD = event, p, state),
    do: {event, p, state}

  def handle_event(:MESSAGE_REACTION_REMOVE = event, p, state),
    do: {event, p, state}

  def handle_event(:MESSAGE_ACK = event, p, state),
    do: {event, p, state}

  def handle_event(:PRESENCE_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(:READY = event, p, state) do
    p.private_channels
    |> Enum.each(fn dm_channel -> ChannelCache.create(dm_channel) end)

    ready_guilds =
      p.guilds
      |> Enum.map(fn guild -> handle_event(:GUILD_CREATE, guild, state) end)

    ready_guilds ++ [{event, p, state}]
  end

  def handle_event(:RESUMED = event, p, state),
    do: {event, p, state}

  def handle_event(:TYPING_START = event, p, state),
    do: {event, p, state}

  def handle_event(:USER_SETTINGS_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(:USER_UPDATE = event, p, state),
    do: {event, UserCache.update(p), state}

  def handle_event(:VOICE_STATE_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(:VOICE_SERVER_UPDATE = event, p, state),
    do: {event, p, state}

  def handle_event(event, p, _state, _pid) do
    Logger.warn "UNHANDLED GATEWAY DISPATCH EVENT TYPE: #{event}, #{inspect p}"
    p
  end
end
