defmodule Mixcord.Shard.Dispatch do
  @moduledoc """
  """

  require Logger

  def handle(payload, state) do
    Logger.debug payload.t
    # TODO: LOWER CASE THESE
    case payload.t do
      :CHANNEL_CREATE ->
        :noop
      :CHANNEL_DELETE ->
        :noop
      :CHANNEL_UPDATE ->
        :noop
      :GUILD_BAN_ADD ->
        :noop
      :BUILD_BAN_REMOVE ->
        :noop
      :GUILD_CREATE ->
        Mixcord.Cache.Guild.create(payload.d)
      :GUILD_DELETE ->
        Mixcord.Cache.Guild.remove(payload.d.id)
      :GUILD_EMOJI_UPDATE ->
        :noop
      :GUILD_INTEGRATIONS_UPDATE ->
        :noop
      :GUILD_MEMBER_ADD ->
        :noop
      :GUILD_MEMBER_CHUNK ->
        :noop
      :GUILD_MEMBER_REMOVE ->
        :noop
      :GUILD_MEMBER_UPDATE ->
        :noop
      :GUILD_ROLE_CREATE ->
        :noop
      :GUILD_ROLE_DELETE ->
        :noop
      :GUILD_ROLE_UPDATE ->
        :noop
      :GUILD_UPDATE ->
        :noop
      :MESSAGE_CREATE ->
        :noop
      :MESSAGE_DELETE ->
        :noop
      :MESSAGE_DELETE_BULK ->
        :noop
      :MESSAGE_UPDATE ->
        :noop
      :PRESENCE_UPDATE ->
        :noop
      :READY ->
        :noop
      :RESUMED ->
        :noop
      :TYPING_START ->
        :noop
      :USER_SETTINGS_UPDATE ->
        :noop
      :USER_UPDATE ->
        :noop
      :VOICE_STATE_UPDATE ->
        :noop
      :VOICE_SERVER_UPDATE ->
        :noop
      _ ->
        Logger.warn "UNHANDLED GATEWAY DISPATCH EVENT TYPE: #{payload.t}"
    end

    state.caller.handle_event({payload.t, payload.d}, state)
  end

end