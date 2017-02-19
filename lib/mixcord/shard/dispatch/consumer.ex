defmodule Mixcord.Shard.Dispatch.Consumer do
  @moduledoc """
  Consumer process for gateway event handling.

  # Consuming Gateway Events
  To handle events, Mixcord uses a GenStage implementation. GenStage is "new" with
  Elixir version 1.4, expanding on the old functionality of GenEvent.

  Mixcord defines the `producer` in the GenStage design. To consume the events you must
  create at least one `consumer` process. For
  an example of this behaviour please see
  [here](https://github.com/Kraigie/mixcord/blob/84502606f570d27dd4450d95a88a796839369bfb/examples/event_consumer.ex).

  It is generally recommended that you spawn a consumer per core. To find this
  number you can use `System.schedulers_online/0`.
  """

  use GenStage
  alias Mixcord.Producer

  @doc """
  Callback used to handle events.

  `Event` is the event name as an atom, and `ws_state` is the current state of
  the websocket that the event was received on. For more information on this please
  see `Mixcord.Shard.Payload.state_map.t`.

  `from` is the process information of the producer from which the demand was received.
  `state` is the internal state of your consumer.
  """
  @callback handle_event(event, state) :: {:ok, Map.t}

  @typedoc """
  Tuple describing the client of a call request.

  `pid` is the PID of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @typedoc """
  The state of the websocket connection for the shard the event occured on.
  """
  @type ws_state :: Map.t

  @typedoc """
  The state of the consumer process.
  """
  @type state :: Map.t

  @type channel_create :: {:CHANNEL_CREATE, {Mixcord.Struct.Channel.t}, ws_state}
  @type channel_delete :: {:CHANNEL_DELETE, {Mixcord.Struct.Channel.t}, ws_state}
  @type channel_update :: {:CHANNEL_UPDATE, {old_channel :: Mixcord.Struct.Channel.t, new_channel :: Mixcord.Struct.Channel.t}, ws_state}
  @type channel_pins_ack :: {:CHANNEL_PINS_ACK, {Map.t}, ws_state}
  @type channel_pins_update :: {:CHANNEL_PINS_UPDATE, {Map.t}, ws_state}
  @type guild_ban_add :: {:GUILD_BAN_ADD, {Mixcord.Struct.User.t}, ws_state}
  @type build_ban_remove :: {:GUILD_BAN_REMOVE, {Mixcord.Struct.User.t}, ws_state}
  @type guild_create :: {:GUILD_CREATE, {new_guild :: Mixcord.Struct.Guild.t}, ws_state}
  @type guild_update :: {:GUILD_CREATE, {old_guild :: Mixcord.Struct.Guild.t, new_guild :: Mixcord.Struct.Guild.t}, ws_state}
  @type guild_delete :: {:GUILD_DELETE, {old_guild :: Mixcord.Struct.Guild.t}, ws_state}
  @type guild_emojis_update :: {:GUILD_EMOJIS_UPDATE, {old_emojis :: [Mixcord.Struct.Emoji.t],  new_emojis :: [Mixcord.Struct.Emoji.t]}, ws_state}
  @type guild_integrations_update :: {:GUILD_INTEGERATIONS_UPDATE, {Map.t}, ws_state}
  @type guild_member_add :: {:GUILD_MEMBER_ADD, {new_member :: Mixcord.Struct.Member.t}, ws_state}
  @type guild_members_chunk :: {:GUILD_MEMBERS_CHUNK, {Map.t}, ws_state}
  @type guild_member_remove :: {:GUILD_MEMBER_REMOVE, {old_member :: Mixcord.Struct.Member.t}, ws_state}
  @type guild_member_update :: {:GUILD_MEMBER_UPDATE, {old_member :: Mixcord.Struct.Member.t, new_member :: Mixcord.Struct.Member.t}, ws_state}
  @type guild_role_create :: {:GUILD_ROLE_CREATE, {new_role :: Mixcord.Struct.Role.t}, ws_state}
  @type guild_role_delete :: {:GUILD_ROLE_DELETE, {old_role :: Mixcord.Struct.Role.t}, ws_state}
  @type guild_role_update :: {:GUILD_ROLE_UPDATE, {old_role :: Mixcord.Struct.Role.t, new_role :: Mixcord.Struct.Role.t}, ws_state}
  @type message_create :: {:MESSAGE_CREATE, {message :: Mixcord.Struct.Message.t}, ws_state}
  @type message_delete :: {:MESSAGE_DELETE, {message :: Mixcord.Struct.Message.t}, ws_state}
  @type message_delete_bulk :: {:MESSAGE_DELETE_BULK, {updated_messages :: [Mixcord.Struct.Message.t]}, ws_state}
  @type message_update :: {:MESSAGE_UPDATE, {updated_message :: Mixcord.Struct.Message.t}, ws_state}
  @type presence_update :: {:PRESENCE_UPDATE, {Map.t}, ws_state}
  @type ready :: {:READY, {Map.t}, ws_state}
  @type resumed :: {:RESUMED, {Map.t}, ws_state}
  @type typing_start :: {:TYPING_START, {Map.t}, ws_state}
  @type user_settings_update :: no_return
  @type user_update :: {:USER_UPDATE, {old_user :: Mixcord.Struct.User.t, new_user :: Mixcord.Struct.User.t}, ws_state}
  @type voice_state_update :: {:VOICE_STATE_UPDATE, {Map.t}, ws_state}
  @type voice_server_update :: {:VOICE_SERVER_UPDATE, {Map.t}, ws_state}

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


  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Mixcord.Shard.Dispatch.Consumer
      alias Mixcord.Shard.Dispatch.Consumer

      def handle_event(_event, state) do
        {:ok, state}
      end

      defoverridable [handle_event: 2]
    end
  end

  def start_link(mod) do
    GenStage.start_link(__MODULE__, %{mod: mod, state: %{}})
  end

  def init(state) do
    producers = ProducerRegistry
    |> Registry.lookup(:pids)
    |> Enum.map(fn {pid, _value} -> pid end)

    {:consumer, state, subscribe_to: producers}
  end

  def handle_events(events, _from, %{mod: mod, state: their_state} = state) do
    for event <- events do
      # TODO: Put user state in our state
      {:ok, _their_state_ret} = mod.handle_event(event, their_state)
    end
    {:noreply, [], state}
    # {:noreply, [], %{state | state: their_state_ret}}
  end

end
