defmodule Nostrum.Consumer do
  @moduledoc """
  Consumer process for gateway event handling.

  # Consuming Gateway Events
  To handle events, Nostrum uses a GenStage implementation. GenStage is "new" with
  Elixir version 1.4, expanding on the old functionality of GenEvent.

  Nostrum defines the `producer` in the GenStage design. To consume the events
  you must create at least one `consumer` process. It is generally recommended
  that you spawn a consumer per core. To find this number you can use
  `System.schedulers_online/0`.

  ## Example
  An example consumer can be found
  [here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer.ex).
  """

  use GenStage

  @doc """
  Callback used to handle events.

  ### Event
  `event` is a tuple describing the event. The tuple will include information in
  the following format:
  ```Elixir
  {event_name, {event_payload(s)}, ws_state}
  ```

  For example, a message create will look like this
  ```Elixir
  {:MESSAGE_CREATE, {Nostrum.Struct.Message.t}, ws_state}
  ```

  In some cases there will be multiple payloads when something is updated, so as
  to include the new and the old versions. In the event of there being two payloads,
  the old payload will always be first, followed by the new payload.
  ```Elixir
  {:USER_UPDATE, {old_user :: Nostrum.Struct.User.t, new_user :: Nostrum.Struct.User.t}, ws_state}
  ```

  For a full listing of events, please see `Nostrum.Consumer.event`.

  ### Websocket State
  `ws_state` is the current state of
  the websocket that the event was received on. For more information on this please
  see `Nostrum.Shard.Payload.state_map.t`.

  ### State
  `state` is the internal state of your consumer.
  """
  @callback handle_event(event, state) :: {:ok, map}

  @typedoc """
  Tuple describing the client of a call request.

  `pid` is the PID of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @typedoc """
  The state of the websocket connection for the shard the event occured on.
  """
  @type ws_state :: map

  @typedoc """
  The state of the consumer process.
  """
  @type state :: map

  @typedoc """
  The two different types of channels - Guild channels and DM channels.
  """
  @type channel :: Nostrum.Struct.Guild.Channel.t | Nostrum.Struct.DMChannel.t

  @type channel_create :: {:CHANNEL_CREATE, {channel}, ws_state}
  @type channel_delete :: {:CHANNEL_DELETE, {channel}, ws_state}
  @type channel_update :: {:CHANNEL_UPDATE, {old_channel :: channel, new_channel :: channel}, ws_state}
  @type channel_pins_ack :: {:CHANNEL_PINS_ACK, {map}, ws_state}
  @type channel_pins_update :: {:CHANNEL_PINS_UPDATE, {map}, ws_state}
  @type guild_ban_add :: {:GUILD_BAN_ADD, {guild_id :: integer, Nostrum.Struct.User.t}, ws_state}
  @type guild_ban_remove :: {:GUILD_BAN_REMOVE, {guild_id :: integer, Nostrum.Struct.User.t}, ws_state}
  @type guild_create :: {:GUILD_CREATE, {new_guild :: Nostrum.Struct.Guild.t}, ws_state}
  @type guild_available :: {:GUILD_AVAILABLE, {new_guild :: Nostrum.Struct.Guild.t}, ws_state}
  @type guild_unavailable :: {:GUILD_UNAVAILABLE, {unavailable_guild :: Nostrum.Struct.Guild.UnavailableGuild.t}, ws_state}
  @type guild_update :: {:GUILD_CREATE, {old_guild :: Nostrum.Struct.Guild.t, new_guild :: Nostrum.Struct.Guild.t}, ws_state}
  @type guild_delete :: {:GUILD_DELETE, {old_guild :: Nostrum.Struct.Guild.t, unavailable :: boolean}, ws_state}
  @type guild_emojis_update :: {:GUILD_EMOJIS_UPDATE, {guild_id :: integer, old_emojis :: [Nostrum.Struct.Message.Emoji.t],  new_emojis :: [Nostrum.Struct.Message.Emoji.t]}, ws_state}
  @type guild_integrations_update :: {:GUILD_INTEGERATIONS_UPDATE, {map}, ws_state}
  @type guild_member_add :: {:GUILD_MEMBER_ADD, {guild_id :: integer, new_member :: Nostrum.Struct.Guild.Member.t}, ws_state}
  @type guild_members_chunk :: {:GUILD_MEMBERS_CHUNK, {map}, ws_state}
  @type guild_member_remove :: {:GUILD_MEMBER_REMOVE, {guild_id :: integer, old_member :: Nostrum.Struct.Guild.Member.t}, ws_state}
  @type guild_member_update :: {:GUILD_MEMBER_UPDATE, {guild_id :: integer, old_member :: Nostrum.Struct.Guild.Member.t, new_member :: Nostrum.Struct.Guild.Member.t}, ws_state}
  @type guild_role_create :: {:GUILD_ROLE_CREATE, {nguild_id :: integer, new_role :: Nostrum.Struct.Guild.Role.t}, ws_state}
  @type guild_role_delete :: {:GUILD_ROLE_DELETE, {guild_id :: integer, old_role :: Nostrum.Struct.Guild.Role.t}, ws_state}
  @type guild_role_update :: {:GUILD_ROLE_UPDATE, {guild_id :: integer, old_role :: Nostrum.Struct.Guild.Role.t, new_role :: Nostrum.Struct.Guild.Role.t}, ws_state}
  @type message_create :: {:MESSAGE_CREATE, {message :: Nostrum.Struct.Message.t}, ws_state}
  @type message_delete :: {:MESSAGE_DELETE, {message :: Nostrum.Struct.Message.t}, ws_state}
  @type message_delete_bulk :: {:MESSAGE_DELETE_BULK, {updated_messages :: [Nostrum.Struct.Message.t]}, ws_state}
  @type message_update :: {:MESSAGE_UPDATE, {updated_message :: Nostrum.Struct.Message.t}, ws_state}
  @type message_reaction_add :: {:MESSAGE_REACTION_ADD, map}
  @type message_reaction_remove :: {:MESSAGE_REACTION_REMOVE, map}
  @type message_ack :: {:MESSAGE_ACK, map}
  @type presence_update :: {:PRESENCE_UPDATE, {map}, ws_state}
  @type ready :: {:READY, {map}, ws_state}
  @type resumed :: {:RESUMED, {map}, ws_state}
  @type typing_start :: {:TYPING_START, {map}, ws_state}
  @type user_settings_update :: no_return
  @type user_update :: {:USER_UPDATE, {old_user :: Nostrum.Struct.User.t, new_user :: Nostrum.Struct.User.t}, ws_state}
  @type voice_state_update :: {:VOICE_STATE_UPDATE, {Nostrum.Struct.VoiceState.t}, ws_state}
  @type voice_server_update :: {:VOICE_SERVER_UPDATE, {map}, ws_state}

  @type event :: channel_create |
    channel_delete |
    channel_update |
    channel_pins_ack |
    channel_pins_update |
    guild_ban_add |
    guild_ban_remove |
    guild_create |
    guild_available |
    guild_unavailable |
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
    message_reaction_add |
    message_reaction_remove |
    message_ack |
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
      @behaviour Nostrum.Consumer
      alias Nostrum.Consumer

      def handle_event(_event, state) do
        {:ok, state}
      end

      defoverridable [handle_event: 2]
    end
  end

  def start_link(mod, state) do
    GenStage.start_link(__MODULE__, %{mod: mod, state: state})
  end

  @doc false
  def init(state) do
    producers =
      CacheStageRegistry
      |> Registry.lookup(:pids)
      |> Enum.map(fn {pid, _value} -> pid end)

    {:consumer, state, subscribe_to: producers}
  end

  @doc false
  def handle_events(events, _from, %{mod: mod, state: their_state} = state) do
    their_new_state = do_event(mod, events, their_state)
    {:noreply, [], %{state | state: their_new_state}}
  end

  defp do_event(_mod, [], state), do: state
  defp do_event(mod, [event | events], state) do
    case mod.handle_event(event, state) do
      {:ok, their_state_ret} -> do_event(mod, events, their_state_ret)
      other -> raise(Nostrum.Error.ConsumerError, found: other)
    end
  end

end
