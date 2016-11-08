defmodule Mixcord.Shard.Dispatch do
  @moduledoc """
  Handles events from Discord's WS connection.

  All events received are handled in their own task. Any handlers you define will
  be run synchronously after the lib handles the event.

  To define your own event define a function like `handle_event/2`.
  The first parameter is a `tuple` containing the event type as an `atom` and the
  payload of the event as a `map`. The second parameter is the current `state` of the shard
  on which the event was received.

  ## Example
  ```elixir
  import Mixcord.Api, only: [create_message: 2]

  def handle_event({:MESSAGE_CREATE, message}, _state) do
    case message.content do
      "PONG" ->
        create_message(message.channel_id, "PING")
      "PING" ->
        create_message(message.channel_id, "PONG")
      _ ->
        create_message(message.channel_id, "I copy and pasted this code")
    end
  end
  ```
  """

  alias Mixcord.Cache.{Guild}
  require Logger

  def handle(payload, state) do
    Logger.debug payload.t
    handle_event({payload.t, payload.d}, state)
    state.caller.handle_event({payload.t, payload.d}, state)
  end

  def handle_event({:CHANNEL_CREATE, p}, state), do: :noop

  def handle_event({:CHANNEL_DELETE, p}, state), do: :noop

  def handle_event({:CHANNEL_UPDATE, p}, state), do: :noop

  def handle_event({:GUILD_BAN_ADD, p}, state), do: :noop

  def handle_event({:BUILD_BAN_REMOVE, p}, state), do: :noop

  def handle_event({:GUILD_CREATE, p}, state), do: Guild.create(p)

  def handle_event({:GUILD_UPDATE, p}, state), do: Guild.update(p)

  def handle_event({:GUILD_DELETE, p}, state), do: Guild.delete(p.id)

  def handle_event({:GUILD_EMOJIS_UPDATE, p}, state), do: Guild.emoji_update(p.guild_id, p.emojis)

  def handle_event({:GUILD_INTEGRATIONS_UPDATE, p}, state), do: :noop

  def handle_event({:GUILD_MEMBER_ADD, p}, state), do: Guild.member_add(p.guild_id, p)

  def handle_event({:GUILD_MEMBER_CHUNK, p}, state) do
    p.members
      |> Enum.map(fn member -> Guild.member_add(p.guild_id, member) end)
  end

  def handle_event({:GUILD_MEMBER_REMOVE, p}, state), do: Guild.member_remove(p.guild_id, p.user)

  def handle_event({:GUILD_MEMBER_UPDATE, p}, state), do: Guild.member_update(p.guild_id, p.user, p.roles)

  def handle_event({:GUILD_ROLE_CREATE, p}, state), do: Guild.role_create(p.guild_id, p.role)

  def handle_event({:GUILD_ROLE_DELETE, p}, state), do: Guild.role_delete(p.guild_id, p.role_id)

  def handle_event({:GUILD_ROLE_UPDATE, p}, state), do: Guild.role_update(p.guild_id, p.role)

  def handle_event({:MESSAGE_CREATE, p}, state), do: :noop

  def handle_event({:MESSAGE_DELETE, p}, state), do: :noop

  def handle_event({:MESSAGE_DELETE_BULK, p}, state), do: :noop

  def handle_event({:MESSAGE_UPDATE, p}, state), do: :noop

  def handle_event({:PRESENCE_UPDATE, p}, state), do: :noop

  def handle_event({:READY, p}, state), do: :noop

  def handle_event({:RESUMED, p}, state), do: :noop

  def handle_event({:TYPING_START, p}, state), do: :noop

  def handle_event({:USER_SETTINGS_UPDATE, p}, state), do: :noop

  def handle_event({:USER_UPDATE, p}, state), do: :noop

  def handle_event({:VOICE_STATE_UPDATE, p}, state), do: :noop

  def handle_event({:VOICE_SERVER_UPDATE, p}, state), do: :noop

  def handle_event({event, p}, state), do: Logger.warn "UNHANDLED GATEWAY DISPATCH EVENT TYPE: #{event}"

end