defmodule Mixcord.Shard.Dispatch do
  @moduledoc false

  alias Mixcord.Cache.{ChannelCache, UserCache}
  alias Mixcord.Cache.Guild.GuildServer
  require Logger

  @large_threshold 250

  _ = """
  Should return {:ok, {event_info}} or {:error, reason}.

  Event info is what we want to send to the consumer, so in the case of a
  message_create it would be {:ok, {message}

  Cache methods are expected to return proper tuples, other methods should return
  structs and let the wrap method put them into the proper format for the
  producer to handle
  """
  def handle(payload, state) do
    Logger.debug payload.t
    wrap(handle_event({payload.t, payload.d}, state))
  end

  def wrap(res) when is_tuple(res), do: res
  def wrap(res), do: {:ok, {res}}

  def handle_event({:CHANNEL_CREATE, %{is_private: true} = p}, _state),
    do: ChannelCache.create(p)
  def handle_event({:CHANNEL_CREATE, p}, _state) do
    :ets.insert(:channel_guild_map, {p.id, p.guild_id})
    GuildServer.channel_create(p.guild_id, p)
  end

  def handle_event({:CHANNEL_DELETE, %{is_private: true} = p}, _state),
    do: ChannelCache.delete(p)
  def handle_event({:CHANNEL_DELETE, p}, _state) do
    :ets.delete(:channel_guild_map, p.id)
    GuildServer.channel_delete(p.guild_id, p.id)
  end

  def handle_event({:CHANNEL_UPDATE, p}, _state),
    do: GuildServer.channel_update(p.guild_id, p)

  def handle_event({:CHANNEL_PINS_ACK, p}, _state),
    do: p

  def handle_event({:CHANNEL_PINS_UPDATE, p}, _state),
    do: p

  def handle_event({:GUILD_BAN_ADD, p}, _state),
    do: p

  def handle_event({:BUILD_BAN_REMOVE, p}, _state),
    do: p

  def handle_event({:GUILD_CREATE, p}, state) do
    if not Map.has_key?(p, :unavailable) or not p.unavailable do
      p.members
      |> Enum.each(fn member -> UserCache.create(member.user) end)
    end

    Enum.each(p.channels, fn channel ->
      :ets.insert(:channel_guild_map, {channel.id, p.id})
    end)

    if p.member_count < @large_threshold do
      GuildServer.create(p)
    else
      GuildServer.create(p, state.shard_pid)
    end
  end

  def handle_event({:GUILD_UPDATE, p}, _state),
    do: GuildServer.update(p)

  def handle_event({:GUILD_DELETE, p}, _state),
    do: GuildServer.delete(p.id)

  def handle_event({:GUILD_EMOJIS_UPDATE, p}, _state),
    do: GuildServer.emoji_update(p.guild_id, p.emojis)

  def handle_event({:GUILD_INTEGRATIONS_UPDATE, p}, _state),
    do: p

  def handle_event({:GUILD_MEMBER_ADD, p}, _state) do
    UserCache.create(p.user)
    GuildServer.member_add(p.guild_id, p)
  end

  def handle_event({:GUILD_MEMBERS_CHUNK, p}, _state) do
    p.members
      |> Enum.each(fn member ->
        UserCache.create(p.user)
        GuildServer.member_add(p.guild_id, member)
      end)
    p
  end

  def handle_event({:GUILD_MEMBER_REMOVE, p}, _state),
    do: GuildServer.member_remove(p.guild_id, p.user)

  # What is this even??
  def handle_event({:GUILD_MEMBER_UPDATE, p}, _state),
    do: GuildServer.member_update(p.guild_id, p.user, p.nick, p.roles)

  def handle_event({:GUILD_ROLE_CREATE, p}, _state),
    do: GuildServer.role_create(p.guild_id, p.role)

  def handle_event({:GUILD_ROLE_DELETE, p}, _state),
    do: GuildServer.role_delete(p.guild_id, p.role_id)

  def handle_event({:GUILD_ROLE_UPDATE, p}, _state),
    do: GuildServer.role_update(p.guild_id, p.role)

  def handle_event({:MESSAGE_CREATE, p}, _state),
    do: p

  def handle_event({:MESSAGE_DELETE, p}, _state),
    do: p

  def handle_event({:MESSAGE_DELETE_BULK, p}, _state),
    do: p

  def handle_event({:MESSAGE_UPDATE, p}, _state),
    do: p

  def handle_event({:MESSAGE_REACTION_ADD, p}, _state),
    do: p

  def handle_event({:MESSAGE_REACTION_REMOVE, p}, _state),
    do: p

  def handle_event({:PRESENCE_UPDATE, p}, _state),
    do: p

  def handle_event({:READY, p}, _state) do
    p.private_channels
      |> Enum.each(fn dm_channel -> ChannelCache.create(dm_channel) end)
    p.guilds
      |> Enum.each(fn guild -> GuildServer.create(guild) end)
    p
  end

  def handle_event({:RESUMED, p}, _state),
    do: p

  def handle_event({:TYPING_START, p}, _state),
    do: p

  def handle_event({:USER_SETTINGS_UPDATE, p}, _state),
    do: p

  def handle_event({:USER_UPDATE, p}, _state),
    do: UserCache.update(p)

  def handle_event({:VOICE_STATE_UPDATE, p}, _state),
    do: p

  def handle_event({:VOICE_SERVER_UPDATE, p}, _state),
    do: p

  def handle_event({event, p}, _state) do
    Logger.warn "UNHANDLED GATEWAY DISPATCH EVENT TYPE: #{event}, #{inspect p}"
    p
  end
end
