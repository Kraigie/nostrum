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

  def handle_event({:CHANNEL_CREATE, payload}, state), do: :noop

  def handle_event({:CHANNEL_DELETE, payload}, state), do: :noop

  def handle_event({:CHANNEL_UPDATE, payload}, state), do: :noop

  def handle_event({:GUILD_BAN_ADD, payload}, state), do: :noop

  def handle_event({:BUILD_BAN_REMOVE, payload}, state), do: :noop

  def handle_event({:GUILD_CREATE, payload}, state), do: Guild.create(payload)

  def handle_event({:GUILD_UPDATE, payload}, state), do: Guild.update(payload)

  def handle_event({:GUILD_DELETE, payload}, state), do: Guild.delete(payload.id)

  def handle_event({:GUILD_EMOJI_UPDATE, payload}, state), do: :noop

  def handle_event({:GUILD_INTEGRATIONS_UPDATE, payload}, state), do: :noop

  def handle_event({:GUILD_MEMBER_ADD, payload}, state), do: :noop

  def handle_event({:GUILD_MEMBER_CHUNK, payload}, state), do: :noop

  def handle_event({:GUILD_MEMBER_REMOVE, payload}, state), do: :noop

  def handle_event({:GUILD_MEMBER_UPDATE, payload}, state), do: :noop

  def handle_event({:GUILD_ROLE_CREATE, payload}, state), do: :noop

  def handle_event({:GUILD_ROLE_DELETE, payload}, state), do: :noop

  def handle_event({:GUILD_ROLE_UPDATE, payload}, state), do: :noop

  def handle_event({:GUILD_UPDATE, payload}, state), do: :noop

  def handle_event({:MESSAGE_CREATE, payload}, state), do: :noop

  def handle_event({:MESSAGE_DELETE, payload}, state), do: :noop

  def handle_event({:MESSAGE_DELETE_BULK, payload}, state), do: :noop

  def handle_event({:MESSAGE_UPDATE, payload}, state), do: :noop

  def handle_event({:PRESENCE_UPDATE, payload}, state), do: :noop

  def handle_event({:READY, payload}, state), do: :noop

  def handle_event({:RESUMED, payload}, state), do: :noop

  def handle_event({:TYPING_START, payload}, state), do: :noop

  def handle_event({:USER_SETTINGS_UPDATE, payload}, state), do: :noop

  def handle_event({:USER_UPDATE, payload}, state), do: :noop

  def handle_event({:VOICE_STATE_UPDATE, payload}, state), do: :noop

  def handle_event({:VOICE_SERVER_UPDATE, payload}, state), do: :noop

  def handle_event({event, payload}, state), do: Logger.warn "UNHANDLED GATEWAY DISPATCH EVENT TYPE: #{event}"

end