defmodule Nostrum.Consumer do
  @moduledoc """
  Consumer process for gateway event handling.

  # Consuming Gateway Events
  To handle events, Nostrum uses a GenStage implementation.

  Nostrum defines the `producer` and `producer_consumer` in the GenStage design.
  To consume the events you must create at least one `consumer` process. It is
  generally recommended that you spawn a consumer per core. To find this number
  you can use `System.schedulers_online/0`.

  Nostrum uses a ConsumerSupervisor to dispatch events, meaning your handlers
  will each be ran in their own seperate task.

  ## Example
  An example consumer can be found
  [here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer.ex).
  """

  use ConsumerSupervisor

  alias Nostrum.Shard.Stage.Cache
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.WSState

  @doc """
  Callback used to handle events.

  ## Event
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

  For a full listing of events, please see `t:Nostrum.Consumer.event/0`.
  """
  @callback handle_event(event) :: any

  @typedoc """
  Tuple describing the client of a call request.

  `pid` is the PID of the caller and `tag` is a unique term used to identify the
  call.
  """
  @type from :: {pid, tag :: term}

  @typedoc """
  State snapshot for the websocket-controlling process that the event occured on.
  """
  @type ws_state :: WSState

  @type channel_create :: {:CHANNEL_CREATE, {Channel.t()}, ws_state}
  @type channel_delete :: {:CHANNEL_DELETE, {Channel.t()}, ws_state}
  @type channel_update ::
          {:CHANNEL_UPDATE, {old_channel :: Channel.t(), new_channel :: Channel.t()}, ws_state}
  @type channel_pins_ack :: {:CHANNEL_PINS_ACK, {map}, ws_state}
  @type channel_pins_update :: {:CHANNEL_PINS_UPDATE, {map}, ws_state}
  @type guild_ban_add ::
          {:GUILD_BAN_ADD, {guild_id :: integer, Nostrum.Struct.User.t()}, ws_state}
  @type guild_ban_remove ::
          {:GUILD_BAN_REMOVE, {guild_id :: integer, Nostrum.Struct.User.t()}, ws_state}
  @type guild_create :: {:GUILD_CREATE, {new_guild :: Nostrum.Struct.Guild.t()}, ws_state}
  @type guild_available :: {:GUILD_AVAILABLE, {new_guild :: Nostrum.Struct.Guild.t()}, ws_state}
  @type guild_unavailable ::
          {:GUILD_UNAVAILABLE, {unavailable_guild :: Nostrum.Struct.Guild.UnavailableGuild.t()},
           ws_state}
  @type guild_update ::
          {:GUILD_CREATE,
           {old_guild :: Nostrum.Struct.Guild.t(), new_guild :: Nostrum.Struct.Guild.t()},
           ws_state}
  @type guild_delete ::
          {:GUILD_DELETE, {old_guild :: Nostrum.Struct.Guild.t(), unavailable :: boolean},
           ws_state}
  @type guild_emojis_update ::
          {:GUILD_EMOJIS_UPDATE,
           {guild_id :: integer, old_emojis :: [Nostrum.Struct.Message.Emoji.t()],
            new_emojis :: [Nostrum.Struct.Message.Emoji.t()]}, ws_state}
  @type guild_integrations_update :: {:GUILD_INTEGERATIONS_UPDATE, {map}, ws_state}
  @type guild_member_add ::
          {:GUILD_MEMBER_ADD,
           {guild_id :: integer, new_member :: Nostrum.Struct.Guild.Member.t()}, ws_state}
  @type guild_members_chunk :: {:GUILD_MEMBERS_CHUNK, {map}, ws_state}
  @type guild_member_remove ::
          {:GUILD_MEMBER_REMOVE,
           {guild_id :: integer, old_member :: Nostrum.Struct.Guild.Member.t()}, ws_state}
  @type guild_member_update ::
          {:GUILD_MEMBER_UPDATE,
           {guild_id :: integer, old_member :: Nostrum.Struct.Guild.Member.t(),
            new_member :: Nostrum.Struct.Guild.Member.t()}, ws_state}
  @type guild_role_create ::
          {:GUILD_ROLE_CREATE, {guild_id :: integer, new_role :: Nostrum.Struct.Guild.Role.t()},
           ws_state}
  @type guild_role_delete ::
          {:GUILD_ROLE_DELETE, {guild_id :: integer, old_role :: Nostrum.Struct.Guild.Role.t()},
           ws_state}
  @type guild_role_update ::
          {:GUILD_ROLE_UPDATE,
           {guild_id :: integer, old_role :: Nostrum.Struct.Guild.Role.t(),
            new_role :: Nostrum.Struct.Guild.Role.t()}, ws_state}
  @type message_create :: {:MESSAGE_CREATE, {message :: Nostrum.Struct.Message.t()}, ws_state}
  @type message_delete :: {:MESSAGE_DELETE, {message :: Nostrum.Struct.Message.t()}, ws_state}
  @type message_delete_bulk ::
          {:MESSAGE_DELETE_BULK, {updated_messages :: [Nostrum.Struct.Message.t()]}, ws_state}
  @type message_update ::
          {:MESSAGE_UPDATE, {updated_message :: Nostrum.Struct.Message.t()}, ws_state}
  @type message_reaction_add :: {:MESSAGE_REACTION_ADD, {map}, ws_state}
  @type message_reaction_remove :: {:MESSAGE_REACTION_REMOVE, {map}, ws_state}
  @type message_reaction_remove_all :: {:MESSAGE_REACTION_REMOVE_ALL, {map}, ws_state}
  @type message_ack :: {:MESSAGE_ACK, {map}, ws_state}
  @type presence_update :: {:PRESENCE_UPDATE, {map}, ws_state}
  @type ready :: {:READY, {map}, ws_state}
  @type resumed :: {:RESUMED, {map}, ws_state}
  @type typing_start :: {:TYPING_START, {map}, ws_state}
  @type user_settings_update :: no_return
  @type user_update ::
          {:USER_UPDATE,
           {old_user :: Nostrum.Struct.User.t(), new_user :: Nostrum.Struct.User.t()}, ws_state}
  @type voice_state_update :: {:VOICE_STATE_UPDATE, {map}, ws_state}
  @type voice_server_update :: {:VOICE_SERVER_UPDATE, {map}, ws_state}
  @type webhooks_update :: {:WEBHOOKS_UPDATE, {map}, ws_state}

  @type event ::
          channel_create
          | channel_delete
          | channel_update
          | channel_pins_ack
          | channel_pins_update
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
          | typing_start
          | user_settings_update
          | user_update
          | voice_state_update
          | voice_server_update
          | webhooks_update

  defmacro __using__(opts) do
    quote location: :keep do
      @behaviour Nostrum.Consumer

      use Task

      alias Nostrum.Consumer

      def handle_event(_event) do
        :ok
      end

      def start_link(event) do
        Task.start_link(fn ->
          __MODULE__.handle_event(event)
        end)
      end

      def child_spec(_arg) do
        spec = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, []}
        }

        # Default to transient restart as permanent isn't allowed.
        # https://github.com/elixir-lang/gen_stage/commit/2b269accba8b8cb0a71333f55e3bd9ce893b40d4
        opts =
          [restart: :transient]
          |> Keyword.merge(unquote(Macro.escape(opts)))

        Supervisor.child_spec(spec, opts)
      end

      defoverridable handle_event: 1, child_spec: 1
    end
  end

  def start_link(mod) do
    ConsumerSupervisor.start_link(__MODULE__, mod)
  end

  @doc false
  def init(mod) do
    children = [mod]

    ConsumerSupervisor.init(children, strategy: :one_for_one, subscribe_to: [Cache])
  end
end
