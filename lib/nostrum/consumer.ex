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
  alias Nostrum.Struct.{Channel, WSState}

  alias Nostrum.Struct.Event.{
    ChannelPinsUpdate,
    GuildBanAdd,
    GuildBanRemove,
    GuildIntegrationsUpdate,
    MessageDelete,
    MessageDeleteBulk,
    MessageReactionAdd,
    MessageReactionRemove,
    MessageReactionRemoveAll,
    MessageReactionRemoveEmoji,
    Ready,
    SpeakingUpdate,
    TypingStart,
    VoiceServerUpdate,
    VoiceState
  }

  @doc """
  Callback used to handle events.

  ## Event
  `event` is a tuple describing the event. The tuple will include information in
  the following format:
  ```Elixir
  {event_name, {event_payload(s)}, WSState.t}
  ```

  For example, a message create will look like this
  ```Elixir
  {:MESSAGE_CREATE, {Nostrum.Struct.Message.t}, WSState.t}
  ```

  In some cases there will be multiple payloads when something is updated, so as
  to include the new and the old versions. In the event of there being two payloads,
  the old payload will always be first, followed by the new payload.
  ```Elixir
  {:USER_UPDATE, {old_user :: Nostrum.Struct.User.t, new_user :: Nostrum.Struct.User.t}, WSState.t()}
  ```

  For a full listing of events, please see `t:Nostrum.Consumer.event/0`.
  """
  @callback handle_event(event) :: any

  @type options :: [option] | []

  @typedoc """
  General process options.

  The `subscribe_to` option should only be set if you want to use your own producer or producer consumer.
  """
  @type option ::
          {:registry, atom()}
          | {:name, Supervisor.name()}
          | {:max_restarts, non_neg_integer()}
          | {:max_seconds, non_neg_integer()}
          | {:subscribe_to, [GenStage.stage() | {GenStage.stage(), keyword()}]}

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
  @type voice_speaking_update :: {:VOICE_SPEAKING_UPDATE, SpeakingUpdate.t(), WSState.t()}
  @type voice_state_update :: {:VOICE_STATE_UPDATE, VoiceState.t(), WSState.t()}
  @type voice_server_update :: {:VOICE_SERVER_UPDATE, VoiceServerUpdate.t(), WSState.t()}
  @type webhooks_update :: {:WEBHOOKS_UPDATE, map, WSState.t()}

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
          | voice_speaking_update
          | voice_state_update
          | voice_server_update
          | webhooks_update

  defmacro __using__(opts) do
    quote location: :keep do
      @behaviour Nostrum.Consumer

      alias Nostrum.Consumer

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

        Supervisor.child_spec(spec, unquote(Macro.escape(opts)))
      end

      def handle_event(_event) do
        :ok
      end

      defoverridable handle_event: 1, child_spec: 1
    end
  end

  @doc ~S"""
  Starts a consumer process.

  `mod` is the name of the module where you define your event callbacks, which should probably be
  the current module which you can get with `__MODULE__`.

  `opts` is a list of general process options. See `t:Nostrum.Consumer.options/0` for more info.
  """
  @spec start_link(module, options) :: Supervisor.on_start()
  def start_link(mod, opts \\ []) do
    {mod_and_opts, cs_opts} =
      case Keyword.pop(opts, :name) do
        {nil, mod_opts} -> {[mod, mod_opts], []}
        {cs_name, mod_opts} -> {[mod, mod_opts], [name: cs_name]}
      end

    ConsumerSupervisor.start_link(__MODULE__, mod_and_opts, cs_opts)
  end

  @doc false
  def init([mod, opts]) do
    default = [strategy: :one_for_one, subscribe_to: [Cache]]

    ConsumerSupervisor.init(
      [
        %{
          id: mod,
          start: {mod, :start_link, []},
          restart: :transient
        }
      ],
      Keyword.merge(default, opts)
    )
  end
end
