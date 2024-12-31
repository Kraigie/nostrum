defmodule Nostrum.Api do
  @moduledoc ~S"""
  Interface for Discord's rest API.

  By default all methods in this module are ran synchronously. If you wish to
  have async rest operations I recommend you execute these functions inside of a
  task.

  **Examples**
  ```elixir
  # Async Task
  t = Task.async fn ->
    Nostrum.Api.Channel.messages(12345678912345, :infinity, {})
  end
  messages = Task.await t

  # A lot of times we don't care about the return value of the function
  Task.start fn ->
    messages = ["in", "the", "end", "it", "doesn't", "even", "matter"]
    Enum.each messages, &Nostrum.Api.Message.create(12345678912345, &1)
  end
  ```

  #### A note about Strings and Ints
  Currently, responses from the REST api will have `id` fields as `string`.
  Everything received from the WS connection will have `id` fields as `int`.

  If you're processing a response from the API and trying to access something in the cache
  based off of an `id` in the response, you will need to convert it to an `int` using
  `String.to_integer/1`. I'm open to suggestions for how this should be handled going forward.

  **Example**
  ```elixir
  messages = Nostrum.Api.Channel.pinned_messages!(12345678912345)

  authors =
    Enum.map messages, fn msg ->
      author_id = String.to_integer(msg.author.id)
      Nostrum.Cache.User.get!(id: author_id)
    end
  ```
  """

  @crlf "\r\n"

  require Logger

  import Nostrum.Api.Helpers, only: [has_files: 1]

  alias Nostrum.Api.Helpers
  alias Nostrum.Api.Ratelimiter
  alias Nostrum.Cache.Me

  alias Nostrum.Struct.{
    Channel,
    Emoji,
    Guild,
    Interaction,
    Invite,
    Message,
    Message.Poll,
    User
  }

  alias Nostrum.Struct.Guild.{AuditLogEntry, Member, Role}

  @typedoc """
  Represents a failed response from the API.

  This occurs when `:gun` fails, or when the API doesn't respond with `200` or `204`.
  """
  @type error :: {:error, Nostrum.Error.ApiError.t()}

  @typedoc """
  Represents a limit used to retrieve messages.

  Integer number of messages, or :infinity to retrieve all messages.
  """
  @type limit :: integer | :infinity

  @typedoc """
  Represents a tuple used to locate messages.

  The first element of the tuple is an atom.
  The second element will be a message_id as an integer.
  The tuple can also be empty to search from the most recent message in the channel
  """
  @type locator ::
          {:before, integer}
          | {:after, integer}
          | {:around, integer}
          | {}

  @typedoc """
  Represents different statuses the bot can have.

    - `:dnd` - Red circle.
    - `:idle` - Yellow circle.
    - `:online` - Green circle.
    - `:invisible` - The bot will appear offline.
  """
  @type status :: :dnd | :idle | :online | :invisible

  @typedoc """
  Represents an emoji for interacting with reaction endpoints.
  """
  @type emoji :: Emoji.t() | Emoji.api_name()

  @typedoc """
  Represents optional parameters for Api functions.

  Each function has documentation regarding what parameters it
  supports or needs.
  """
  @type options :: keyword | map

  @typedoc """
  Represents which mentions to allow in a message.

  This can be sent on its own or in a list to allow multiple types of
  mentions in a message, see `t:allowed_mentions/0` for details.
  """
  @typedoc since: "0.7.0"
  @type allowed_mention ::
          :all
          | :none
          | :everyone
          | :users
          | :roles
          | {:users, [User.id()]}
          | {:roles, [Role.id()]}

  @typedoc """
  Represents mentions to allow in a message.

  With this option you can control when content from a message should trigger a ping.
  Consider using this option when you are going to display user generated content.

  ### Allowed values
    * `:all` (default) - Ping everything as usual
    * `:none` - Nobody will be pinged
    * `:everyone` - Allows to ping @here and @everyone
    * `:users` - Allows to ping users
    * `:roles` - Allows to ping roles
    * `{:users, list}` - Allows to ping list of users. Can contain up to 100 ids of users.
    * `{:roles, list}` - Allows to ping list of roles. Can contain up to 100 ids of roles.
    * list - a list containing the values above.
  """
  @typedoc since: "0.7.0"
  @type allowed_mentions :: allowed_mention | [allowed_mention]

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.update_shard_status/5` directly instead.
  """
  defdelegate update_shard_status(pid, status, game, type \\ 0, stream \\ nil),
    to: Nostrum.Api.Self

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.update_status/4` directly instead.
  """
  defdelegate update_status(status, game, type \\ 0, stream \\ nil),
    to: Nostrum.Api.Self

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.update_voice_state/4` directly instead.
  """
  defdelegate update_voice_state(guild_id, channel_id, self_mute \\ false, self_deaf \\ false),
    to: Nostrum.Api.Self

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.create/2` directly instead.
  """
  defdelegate create_message(channel_id, options),
    to: Nostrum.Api.Message,
    as: :create

  @doc ~S"""
  Same as `create_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_message!(Channel.id() | Message.t(), options | String.t()) ::
          no_return | Message.t()
  def create_message!(channel_id, options) do
    create_message(channel_id, options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.edit/3` directly instead.
  """
  defdelegate edit_message(channel_id, message_id, options),
    to: Nostrum.Api.Message,
    as: :edit

  @doc ~S"""
  Same as `edit_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec edit_message!(Channel.id(), Message.id(), options) :: no_return | Message.t()
  def edit_message!(channel_id, message_id, options) do
    edit_message(channel_id, message_id, options)
    |> bangify
  end

  @doc ~S"""
  Same as `edit_message/3`, but takes a `Nostrum.Struct.Message` instead of a
  `channel_id` and `message_id`.
  """
  @spec edit_message(Message.t(), options) :: error | {:ok, Message.t()}
  def edit_message(%Message{id: id, channel_id: c_id}, options) do
    edit_message(c_id, id, options)
  end

  @doc ~S"""
  Same as `edit_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec edit_message!(Message.t(), options) :: no_return | Message.t()
  def edit_message!(message, options) do
    edit_message(message, options)
    |> bangify
  end

  @doc ~S"""
  Same as `delete_message/2`, but takes a `Nostrum.Struct.Message` instead of a
  `channel_id` and `message_id`.
  """
  @spec delete_message(Message.t()) :: error | {:ok}
  def delete_message(%Message{id: id, channel_id: c_id}) do
    delete_message(c_id, id)
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.delete/2` directly instead.
  """
  defdelegate delete_message(channel_id, message_id),
    to: Nostrum.Api.Message,
    as: :delete

  @doc ~S"""
  Same as `delete_message/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_message!(Message.t()) :: error | {:ok}
  def delete_message!(%Message{id: id, channel_id: c_id}) do
    delete_message(c_id, id)
    |> bangify
  end

  @doc ~S"""
  Same as `delete_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_message!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_message!(channel_id, message_id) do
    delete_message(channel_id, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.react/3` directly instead.
  """
  defdelegate create_reaction(channel_id, message_id, emoji),
    to: Nostrum.Api.Message,
    as: :react

  @doc ~S"""
  Same as `create_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_reaction!(Channel.id(), Message.id(), emoji) :: no_return | {:ok}
  def create_reaction!(channel_id, message_id, emoji) do
    create_reaction(channel_id, message_id, emoji)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.unreact/3` directly instead.
  """
  defdelegate delete_own_reaction(channel_id, message_id, emoji),
    to: Nostrum.Api.Message,
    as: :unreact

  @doc ~S"""
  Same as `delete_own_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_own_reaction!(Channel.id(), Message.id(), emoji) :: no_return | {:ok}
  def delete_own_reaction!(channel_id, message_id, emoji) do
    delete_own_reaction(channel_id, message_id, emoji)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.delete_user_reaction/4` directly instead.
  """
  defdelegate delete_user_reaction(channel_id, message_id, emoji, user_id),
    to: Nostrum.Api.Message

  @doc ~S"""
  Same as `delete_user_reaction/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_user_reaction!(Channel.id(), Message.id(), emoji, User.id()) :: no_return | {:ok}
  def delete_user_reaction!(channel_id, message_id, emoji, user_id) do
    delete_user_reaction(channel_id, message_id, emoji, user_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.delete_emoji_reactions/3` directly instead.
  """
  defdelegate delete_reaction(channel_id, message_id, emoji),
    to: Nostrum.Api.Message,
    as: :delete_emoji_reactions

  @doc ~S"""
  Same as `delete_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_reaction!(Channel.id(), Message.id(), emoji) :: no_return | {:ok}
  def delete_reaction!(channel_id, message_id, emoji) do
    delete_reaction(channel_id, message_id, emoji)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.reactions/4` directly instead.
  """
  defdelegate get_reactions(channel_id, message_id, emoji, params \\ []),
    to: Nostrum.Api.Message,
    as: :reactions

  @doc ~S"""
  Same as `get_reactions/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_reactions!(Channel.id(), Message.id(), emoji, keyword()) :: no_return | [User.t()]
  def get_reactions!(channel_id, message_id, emoji, params \\ []) do
    get_reactions(channel_id, message_id, emoji, params)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.clear_reactions/2` directly instead.
  """
  defdelegate delete_all_reactions(channel_id, message_id),
    to: Nostrum.Api.Message,
    as: :clear_reactions

  @doc ~S"""
  Same as `delete_all_reactions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_all_reactions!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_all_reactions!(channel_id, message_id) do
    delete_all_reactions(channel_id, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Poll.answer_voters/4` directly instead.
  """
  defdelegate get_poll_answer_voters(channel_id, message_id, answer_id, params \\ []),
    to: Nostrum.Api.Poll,
    as: :answer_voters

  @doc ~S"""
  Same as `get_poll_answer_voters/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_poll_answer_voters!(Channel.id(), Message.id(), Poll.Answer.answer_id()) :: [User.t()]
  def get_poll_answer_voters!(channel_id, message_id, answer_id, params \\ []) do
    get_poll_answer_voters(channel_id, message_id, answer_id, params)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Poll.expire/2` directly instead.
  """
  defdelegate expire_poll(channel_id, message_id),
    to: Nostrum.Api.Poll,
    as: :expire

  @doc ~S"""
  Same as `expire_poll/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec expire_poll!(Channel.id(), Message.id()) :: Message.t()
  def expire_poll!(channel_id, message_id) do
    expire_poll(channel_id, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.get/1` directly instead.
  """
  defdelegate get_channel(channel_id),
    to: Nostrum.Api.Channel,
    as: :get

  @doc ~S"""
  Same as `get_channel/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_channel!(Channel.id()) :: no_return | Channel.t()
  def get_channel!(channel_id) do
    get_channel(channel_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.modify/3` directly instead.
  """
  defdelegate modify_channel(channel_id, options, reason \\ nil),
    to: Nostrum.Api.Channel,
    as: :modify

  @doc ~S"""
  Same as `modify_channel/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_channel!(Channel.id(), options, AuditLogEntry.reason()) :: no_return | Channel.t()
  def modify_channel!(channel_id, options, reason \\ nil) do
    modify_channel(channel_id, options, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.delete/2` directly instead.
  """
  defdelegate delete_channel(channel_id, reason \\ nil),
    to: Nostrum.Api.Channel,
    as: :delete

  @doc ~S"""
  Same as `delete_channel/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_channel!(Channel.id(), AuditLogEntry.reason()) :: no_return | Channel.t()
  def delete_channel!(channel_id, reason \\ nil) do
    delete_channel(channel_id, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.messages/3` directly instead.
  """
  defdelegate get_channel_messages(channel_id, limit, locator \\ {}),
    to: Nostrum.Api.Channel,
    as: :messages

  @doc ~S"""
  Same as `get_channel_messages/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_channel_messages!(Channel.id(), limit, locator) :: no_return | [Message.t()]
  def get_channel_messages!(channel_id, limit, locator \\ {}) do
    get_channel_messages(channel_id, limit, locator)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Message.get/2` directly instead.
  """
  defdelegate get_channel_message(channel_id, message_id),
    to: Nostrum.Api.Message,
    as: :get

  @doc ~S"""
  Same as `get_channel_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_channel_message!(Channel.id(), Message.id()) :: no_return | Message.t()
  def get_channel_message!(channel_id, message_id) do
    get_channel_message(channel_id, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.bulk_delete_messages/3` directly instead.
  """
  defdelegate bulk_delete_messages(channel_id, messages, filter),
    to: Nostrum.Api.Channel

  @doc """
  Same as `bulk_delete_messages/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec bulk_delete_messages!(integer, [Nostrum.Struct.Message.id()], boolean) ::
          no_return | {:ok}
  def bulk_delete_messages!(channel_id, messages, filter \\ true) do
    bulk_delete_messages(channel_id, messages, filter)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.edit_permissions/4` directly instead.
  """
  defdelegate edit_channel_permissions(channel_id, overwrite_id, permission_info, reason \\ nil),
    to: Nostrum.Api.Channel,
    as: :edit_permissions

  @doc """
  Same as `edit_channel_permissions/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec edit_channel_permissions!(
          integer,
          integer,
          %{
            required(:type) => String.t(),
            optional(:allow) => integer,
            optional(:deny) => integer
          },
          AuditLogEntry.reason()
        ) :: no_return | {:ok}
  def edit_channel_permissions!(channel_id, overwrite_id, permission_info, reason \\ nil) do
    edit_channel_permissions(channel_id, overwrite_id, permission_info, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.delete_permissions/3` directly instead.
  """
  defdelegate delete_channel_permissions(channel_id, overwrite_id, reason \\ nil),
    to: Nostrum.Api.Channel,
    as: :delete_permissions

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Invite.channel_invites/1` directly instead.
  """
  defdelegate get_channel_invites(channel_id),
    to: Nostrum.Api.Invite,
    as: :channel_invites

  @doc ~S"""
  Same as `get_channel_invites/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_channel_invites!(Channel.id()) :: no_return | [Invite.detailed_invite()]
  def get_channel_invites!(channel_id) do
    get_channel_invites(channel_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Invite.create/3` directly instead.
  """
  defdelegate create_channel_invite(channel_id, options \\ [], reason \\ nil),
    to: Nostrum.Api.Invite,
    as: :create

  @doc ~S"""
  Same as `create_channel_invite/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_channel_invite!(Channel.id(), options, AuditLogEntry.reason()) ::
          no_return | Invite.detailed_invite()
  def create_channel_invite!(channel_id, options \\ [], reason \\ nil) do
    create_channel_invite(channel_id, options, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.start_typing/1` directly instead.
  """
  defdelegate start_typing(channel_id),
    to: Nostrum.Api.Channel

  @doc """
  Same as `start_typing/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec start_typing!(integer) :: no_return | {:ok}
  def start_typing!(channel_id) do
    start_typing(channel_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.pinned_messages/1` directly instead.
  """
  defdelegate get_pinned_messages(channel_id),
    to: Nostrum.Api.Channel,
    as: :pinned_messages

  @doc ~S"""
  Same as `get_pinned_messages/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_pinned_messages!(Channel.id()) :: no_return | [Message.t()]
  def get_pinned_messages!(channel_id) do
    get_pinned_messages(channel_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.pin_message/2` directly instead.
  """
  defdelegate add_pinned_channel_message(channel_id, message_id),
    to: Nostrum.Api.Channel,
    as: :pin_message

  @doc ~S"""
  Same as `add_pinned_channel_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec add_pinned_channel_message!(Channel.id(), Message.id()) :: no_return | {:ok}
  def add_pinned_channel_message!(channel_id, message_id) do
    add_pinned_channel_message(channel_id, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.unpin_message/2` directly instead.
  """
  defdelegate delete_pinned_channel_message(channel_id, message_id),
    to: Nostrum.Api.Channel,
    as: :unpin_message

  @doc ~S"""
  Same as `delete_pinned_channel_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_pinned_channel_message!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_pinned_channel_message!(channel_id, message_id) do
    delete_pinned_channel_message(channel_id, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.emojis/1` directly instead.
  """
  defdelegate list_guild_emojis(guild_id),
    to: Nostrum.Api.Guild,
    as: :emojis

  @doc ~S"""
  Same as `list_guild_emojis/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec list_guild_emojis!(Guild.id()) :: no_return | [Emoji.t()]
  def list_guild_emojis!(guild_id) do
    list_guild_emojis(guild_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.emoji/2` directly instead.
  """
  defdelegate get_guild_emoji(guild_id, emoji_id),
    to: Nostrum.Api.Guild,
    as: :emoji

  @doc ~S"""
  Same as `get_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild_emoji!(Guild.id(), Emoji.id()) :: no_return | Emoji.t()
  def get_guild_emoji!(guild_id, emoji_id) do
    get_guild_emoji(guild_id, emoji_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.create_emoji/3` directly instead.
  """
  defdelegate create_guild_emoji(guild_id, options, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :create_emoji

  @doc ~S"""
  Same as `create_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_guild_emoji!(Guild.id(), options, AuditLogEntry.reason()) :: no_return | Emoji.t()
  def create_guild_emoji!(guild_id, params, reason \\ nil) do
    create_guild_emoji(guild_id, params, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_emoji/4` directly instead.
  """
  defdelegate modify_guild_emoji(guild_id, emoji_id, options \\ %{}, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :modify_emoji

  @doc ~S"""
  Same as `modify_guild_emoji/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_guild_emoji!(Guild.id(), Emoji.id(), options, AuditLogEntry.reason()) ::
          no_return | Emoji.t()
  def modify_guild_emoji!(guild_id, emoji_id, options, reason \\ nil) do
    modify_guild_emoji(guild_id, emoji_id, options, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.delete_emoji/3` directly instead.
  """
  defdelegate delete_guild_emoji(guild_id, emoji_id, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :delete_emoji

  @doc ~S"""
  Same as `delete_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_guild_emoji!(Guild.id(), Emoji.id(), AuditLogEntry.reason()) :: no_return | {:ok}
  def delete_guild_emoji!(guild_id, emoji_id, reason \\ nil) do
    delete_guild_emoji(guild_id, emoji_id, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.get/1` directly instead.
  """
  defdelegate get_sticker(sticker_id),
    to: Nostrum.Api.Sticker,
    as: :get

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.list/1` directly instead.
  """
  defdelegate list_guild_stickers(guild_id),
    to: Nostrum.Api.Sticker,
    as: :list

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.get/2` directly instead.
  """
  defdelegate get_guild_sticker(guild_id, sticker_id),
    to: Nostrum.Api.Sticker,
    as: :get

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.create/6` directly instead.
  """
  defdelegate create_guild_sticker(guild_id, name, description, tags, file, reason \\ nil),
    to: Nostrum.Api.Sticker,
    as: :create

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.modify/3` directly instead.
  """
  defdelegate modify_guild_sticker(guild_id, sticker_id, options),
    to: Nostrum.Api.Sticker,
    as: :modify

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.delete/2` directly instead.
  """
  defdelegate delete_guild_sticker(guild_id, sticker_id),
    to: Nostrum.Api.Sticker,
    as: :delete

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Sticker.packs/0` directly instead.
  """
  defdelegate get_sticker_packs,
    to: Nostrum.Api.Sticker,
    as: :packs

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.audit_log/2` directly instead.
  """
  defdelegate get_guild_audit_log(guild_id, options \\ []),
    to: Nostrum.Api.Guild,
    as: :audit_log

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.get/1` directly instead.
  """
  defdelegate get_guild(guild_id),
    to: Nostrum.Api.Guild,
    as: :get

  @doc """
  Same as `get_guild/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild!(Guild.id()) :: no_return | Guild.rest_guild()
  def get_guild!(guild_id) do
    get_guild(guild_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify/3` directly instead.
  """
  defdelegate modify_guild(guild_id, options \\ [], reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :modify

  @doc """
  Same as `modify_guild/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_guild!(Guild.id(), options) :: no_return | Guild.rest_guild()
  def modify_guild!(guild_id, options \\ []) do
    modify_guild(guild_id, options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.delete/1` directly instead.
  """
  defdelegate delete_guild(guild_id),
    to: Nostrum.Api.Guild,
    as: :delete

  @doc ~S"""
  Same as `delete_guild/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_guild!(Guild.id()) :: no_return | {:ok}
  def delete_guild!(guild_id) do
    delete_guild(guild_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.channels/1` directly instead.
  """
  defdelegate get_guild_channels(guild_id),
    to: Nostrum.Api.Guild,
    as: :channels

  @doc ~S"""
  Same as `get_guild_channels/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild_channels!(Guild.id()) :: no_return | [Channel.guild_channel()]
  def get_guild_channels!(guild_id) do
    get_guild_channels(guild_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.create/2` directly instead.
  """
  defdelegate create_guild_channel(guild_id, options),
    to: Nostrum.Api.Channel,
    as: :create

  @doc ~S"""
  Same as `create_guild_channel/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_guild_channel!(Guild.id(), options) :: no_return | Channel.guild_channel()
  def create_guild_channel!(guild_id, options) do
    create_guild_channel(guild_id, options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_channel_positions/2` directly instead.
  """
  defdelegate modify_guild_channel_positions(guild_id, positions),
    to: Nostrum.Api.Guild,
    as: :modify_channel_positions

  @doc ~S"""
  Same as `modify_guild_channel_positions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_guild_channel_positions!(Guild.id(), [%{id: integer, position: integer}]) ::
          no_return | {:ok}
  def modify_guild_channel_positions!(guild_id, positions) do
    modify_guild_channel_positions(guild_id, positions)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.member/2` directly instead.
  """
  defdelegate get_guild_member(guild_id, user_id),
    to: Nostrum.Api.Guild,
    as: :member

  @doc """
  Same as `get_guild_member/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild_member!(Guild.id(), User.id()) :: no_return | Member.t()
  def get_guild_member!(guild_id, user_id) do
    get_guild_member(guild_id, user_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.members/2` directly instead.
  """
  defdelegate list_guild_members(guild_id, options \\ %{}),
    to: Nostrum.Api.Guild,
    as: :members

  @doc """
  Same as `list_guild_members/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec list_guild_members!(Guild.id(), options) :: no_return | [Member.t()]
  def list_guild_members!(guild_id, options \\ %{}) do
    list_guild_members(guild_id, options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.add_member/3` directly instead.
  """
  defdelegate add_guild_member(guild_id, user_id, options),
    to: Nostrum.Api.Guild,
    as: :add_member

  @doc """
  Same as `add_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec add_guild_member!(Guild.id(), User.id(), options) :: no_return | Member.t() | {:ok}
  def add_guild_member!(guild_id, user_id, options) do
    add_guild_member(guild_id, user_id, options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_member/4` directly instead.
  """
  defdelegate modify_guild_member(guild_id, user_id, options \\ %{}, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :modify_member

  @doc """
  Same as `modify_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_guild_member!(Guild.id(), User.id(), options, AuditLogEntry.reason()) ::
          error | {:ok}
  def modify_guild_member!(guild_id, user_id, options \\ %{}, reason \\ nil) do
    modify_guild_member(guild_id, user_id, options, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_self_nick/2` directly instead.
  """
  defdelegate modify_current_user_nick(guild_id, options \\ %{}),
    to: Nostrum.Api.Guild,
    as: :modify_self_nick

  @doc """
  Same as `modify_current_user_nick/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_current_user_nick!(Guild.id(), options) :: no_return | %{nick: String.t()}
  def modify_current_user_nick!(guild_id, options \\ %{}) do
    modify_current_user_nick(guild_id, options)
    |> bangify()
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.add_member_role/4` directly instead.
  """
  defdelegate add_guild_member_role(guild_id, user_id, role_id, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :add_member_role

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.remove_member_role/4` directly instead.
  """
  defdelegate remove_guild_member_role(guild_id, user_id, role_id, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :remove_member_role

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.kick_member/3` directly instead.
  """
  defdelegate remove_guild_member(guild_id, user_id, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :kick_member

  @doc """
  Same as `remove_guild_member/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec remove_guild_member!(Guild.id(), User.id(), AuditLogEntry.reason()) :: no_return | {:ok}
  def remove_guild_member!(guild_id, user_id, reason \\ nil) do
    remove_guild_member(guild_id, user_id, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.ban/2` directly instead.
  """
  defdelegate get_guild_ban(guild_id, user_id),
    to: Nostrum.Api.Guild,
    as: :ban

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.bans/1` directly instead.
  """
  defdelegate get_guild_bans(guild_id),
    to: Nostrum.Api.Guild,
    as: :bans

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.ban_member/4` directly instead.
  """
  defdelegate create_guild_ban(guild_id, user_id, days_to_delete, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :ban_member

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.unban_member/3` directly instead.
  """
  defdelegate remove_guild_ban(guild_id, user_id, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :unban_member

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.roles/1` directly instead.
  """
  defdelegate get_guild_roles(guild_id),
    to: Nostrum.Api.Guild,
    as: :roles

  @doc ~S"""
  Same as `get_guild_roles/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild_roles!(Guild.id()) :: no_return | [Role.t()]
  def get_guild_roles!(guild_id) do
    get_guild_roles(guild_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.create_role/3` directly instead.
  """
  defdelegate create_guild_role(guild_id, options, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :create_role

  @doc ~S"""
  Same as `create_guild_role/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_guild_role!(Guild.id(), options, AuditLogEntry.reason()) :: no_return | Role.t()
  def create_guild_role!(guild_id, options, reason \\ nil) do
    create_guild_role(guild_id, options, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_role_positions/3` directly instead.
  """
  defdelegate modify_guild_role_positions(guild_id, positions, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :modify_role_positions

  @doc ~S"""
  Same as `modify_guild_role_positions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_guild_role_positions!(
          Guild.id(),
          [%{id: Role.id(), position: integer}],
          AuditLogEntry.reason()
        ) :: no_return | [Role.t()]
  def modify_guild_role_positions!(guild_id, positions, reason \\ nil) do
    modify_guild_role_positions(guild_id, positions, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_role/4` directly instead.
  """
  defdelegate modify_guild_role(guild_id, role_id, options, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :modify_role

  @doc ~S"""
  Same as `modify_guild_role/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_guild_role!(Guild.id(), Role.id(), options, AuditLogEntry.reason()) ::
          no_return | Role.t()
  def modify_guild_role!(guild_id, role_id, options, reason \\ nil) do
    modify_guild_role(guild_id, role_id, options, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.delete_role/3` directly instead.
  """
  defdelegate delete_guild_role(guild_id, role_id, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :delete_role

  @doc ~S"""
  Same as `delete_guild_role/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_guild_role!(Guild.id(), Role.id(), AuditLogEntry.reason()) :: no_return | {:ok}
  def delete_guild_role!(guild_id, role_id, reason \\ nil) do
    delete_guild_role(guild_id, role_id, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.estimate_prune_count/2` directly instead.
  """
  defdelegate get_guild_prune_count(guild_id, days),
    to: Nostrum.Api.Guild,
    as: :estimate_prune_count

  @doc ~S"""
  Same as `get_guild_prune_count/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild_prune_count!(Guild.id(), 1..30) :: no_return | %{pruned: integer}
  def get_guild_prune_count!(guild_id, days) do
    get_guild_prune_count(guild_id, days)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.begin_prune/3` directly instead.
  """
  defdelegate begin_guild_prune(guild_id, days, reason \\ nil),
    to: Nostrum.Api.Guild,
    as: :begin_prune

  @doc ~S"""
  Same as `begin_guild_prune/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec begin_guild_prune!(Guild.id(), 1..30, AuditLogEntry.reason()) ::
          no_return | %{pruned: integer}
  def begin_guild_prune!(guild_id, days, reason) do
    begin_guild_prune(guild_id, days, reason)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.voice_region/1` directly instead.
  """
  defdelegate get_voice_region(guild_id),
    to: Nostrum.Api.Guild,
    as: :voice_region

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Invite.guild_invites/1` directly instead.
  """
  defdelegate get_guild_invites(guild_id),
    to: Nostrum.Api.Invite,
    as: :guild_invites

  @doc ~S"""
  Same as `get_guild_invites/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_guild_invites!(Guild.id()) :: no_return | [Invite.detailed_invite()]
  def get_guild_invites!(guild_id) do
    get_guild_invites(guild_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.integrations/1` directly instead.
  """
  defdelegate get_guild_integrations(guild_id),
    to: Nostrum.Api.Guild,
    as: :integrations

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.create_integration/2` directly instead.
  """
  defdelegate create_guild_integrations(guild_id, options),
    to: Nostrum.Api.Guild,
    as: :create_integration

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_integration/3` directly instead.
  """
  defdelegate modify_guild_integrations(guild_id, integration_id, options),
    to: Nostrum.Api.Guild,
    as: :modify_integration

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.delete_integration/2` directly instead.
  """
  defdelegate delete_guild_integrations(guild_id, integration_id),
    to: Nostrum.Api.Guild,
    as: :delete_integration

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.sync_integration/2` directly instead.
  """
  defdelegate sync_guild_integrations(guild_id, integration_id),
    to: Nostrum.Api.Guild,
    as: :sync_integration

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.widget/1` directly instead.
  """
  defdelegate get_guild_widget(guild_id),
    to: Nostrum.Api.Guild,
    as: :widget

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.modify_widget/2` directly instead.
  """
  defdelegate modify_guild_widget(guild_id, options),
    to: Nostrum.Api.Guild,
    as: :modify_widget

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ScheduledEvent.create/3` directly instead.
  """
  defdelegate create_guild_scheduled_event(guild_id, reason \\ nil, options),
    to: Nostrum.Api.ScheduledEvent,
    as: :create

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.scheduled_events/1` directly instead.
  """
  defdelegate get_guild_scheduled_events(guild_id),
    to: Nostrum.Api.Guild,
    as: :scheduled_events

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ScheduledEvent.get/2` directly instead.
  """
  defdelegate get_guild_scheduled_event(guild_id, event_id),
    to: Nostrum.Api.ScheduledEvent,
    as: :get

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ScheduledEvent.delete/2` directly instead.
  """
  defdelegate delete_guild_scheduled_event(guild_id, event_id),
    to: Nostrum.Api.ScheduledEvent,
    as: :delete

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ScheduledEvent.modify/4` directly instead.
  """
  defdelegate modify_guild_scheduled_event(guild_id, event_id, reason \\ nil, options),
    to: Nostrum.Api.ScheduledEvent,
    as: :modify

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ScheduledEvent.users/3` directly instead.
  """
  defdelegate get_guild_scheduled_event_users(guild_id, event_id, params \\ []),
    to: Nostrum.Api.ScheduledEvent,
    as: :users

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Invite.get/2` directly instead.
  """
  defdelegate get_invite(invite_code, options \\ []),
    to: Nostrum.Api.Invite,
    as: :get

  @doc ~S"""
  Same as `get_invite/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_invite!(Invite.code(), options) :: no_return | Invite.simple_invite()
  def get_invite!(invite_code, options \\ []) do
    get_invite(invite_code, options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Invite.delete/1` directly instead.
  """
  defdelegate delete_invite(invite_code),
    to: Nostrum.Api.Invite,
    as: :delete

  @doc ~S"""
  Same as `delete_invite/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_invite!(Invite.code()) :: no_return | Invite.simple_invite()
  def delete_invite!(invite_code) do
    delete_invite(invite_code)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.User.get/1` directly instead.
  """
  defdelegate get_user(user_id),
    to: Nostrum.Api.User,
    as: :get

  @doc """
  Same as `get_user/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_user!(User.id()) :: no_return | User.t()
  def get_user!(user_id) do
    get_user(user_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.get/0` directly instead.
  """
  defdelegate get_current_user,
    to: Nostrum.Api.Self,
    as: :get

  @doc """
  Same as `get_current_user/0`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_current_user!() :: no_return | User.t()
  def get_current_user! do
    get_current_user()
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.modify/1` directly instead.
  """
  defdelegate modify_current_user(options),
    to: Nostrum.Api.Self,
    as: :modify

  @doc """
  Same as `modify_current_user/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec modify_current_user!(options) :: no_return | User.t()
  def modify_current_user!(options) do
    modify_current_user(options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.guilds/1` directly instead.
  """
  defdelegate get_current_user_guilds(options \\ []),
    to: Nostrum.Api.Self,
    as: :guilds

  @doc ~S"""
  Same as `get_current_user_guilds/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_current_user_guilds!(options) :: no_return | [Guild.user_guild()]
  def get_current_user_guilds!(options \\ []) do
    get_current_user_guilds(options)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.leave/1` directly instead.
  """
  defdelegate leave_guild(guild_id),
    to: Nostrum.Api.Guild,
    as: :leave

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.dms/0` directly instead.
  """
  defdelegate get_user_dms,
    to: Nostrum.Api.Self,
    as: :dms

  @doc ~S"""
  Same as `get_user_dms/0`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec get_user_dms!() :: no_return | [Channel.dm_channel()]
  def get_user_dms! do
    get_user_dms()
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.User.create_dm/1` directly instead.
  """
  defdelegate create_dm(user_id),
    to: Nostrum.Api.User

  @doc ~S"""
  Same as `create_dm/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_dm!(User.id()) :: no_return | Channel.dm_channel()
  def create_dm!(user_id) do
    create_dm(user_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.User.create_group_dm/2` directly instead.
  """
  defdelegate create_group_dm(access_tokens, nicks),
    to: Nostrum.Api.User

  @doc ~S"""
  Same as `create_group_dm/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_group_dm!([String.t()], %{optional(User.id()) => String.t()}) ::
          no_return | Channel.group_dm_channel()
  def create_group_dm!(access_tokens, nicks) do
    create_group_dm(access_tokens, nicks)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.connections/0` directly instead.
  """
  defdelegate get_user_connections,
    to: Nostrum.Api.Self,
    as: :connections

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.voice_regions/0` directly instead.
  """
  defdelegate list_voice_regions,
    to: Nostrum.Api.Guild,
    as: :voice_regions

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.create/3` directly instead.
  """
  defdelegate create_webhook(channel_id, args, reason \\ nil),
    to: Nostrum.Api.Webhook,
    as: :create

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.get_message/2` directly instead.
  """
  defdelegate get_webhook_message(webhook, message_id),
    to: Nostrum.Api.Webhook,
    as: :get_message

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Channel.webhooks/1` directly instead.
  """
  defdelegate get_channel_webhooks(channel_id),
    to: Nostrum.Api.Channel,
    as: :webhooks

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Guild.webhooks/1` directly instead.
  """
  defdelegate get_guild_webhooks(guild_id),
    to: Nostrum.Api.Guild,
    as: :webhooks

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.get/1` directly instead.
  """
  defdelegate get_webhook(webhook_id),
    to: Nostrum.Api.Webhook,
    as: :get

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.get_with_token/2` directly instead.
  """
  defdelegate get_webhook_with_token(webhook_id, webhook_token),
    to: Nostrum.Api.Webhook,
    as: :get_with_token

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.modify/3` directly instead.
  """
  defdelegate modify_webhook(webhook_id, args, reason \\ nil),
    to: Nostrum.Api.Webhook,
    as: :modify

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.modify_with_token/4` directly instead.
  """
  defdelegate modify_webhook_with_token(webhook_id, webhook_token, args, reason \\ nil),
    to: Nostrum.Api.Webhook,
    as: :modify_with_token

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.delete/2` directly instead.
  """
  defdelegate delete_webhook(webhook_id, reason \\ nil),
    to: Nostrum.Api.Webhook,
    as: :delete

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.execute/4` directly instead.
  """
  defdelegate execute_webhook(webhook_id, webhook_token, args, wait \\ false),
    to: Nostrum.Api.Webhook,
    as: :execute

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.edit_message/4` directly instead.
  """
  defdelegate edit_webhook_message(webhook_id, webhook_token, message_id, args),
    to: Nostrum.Api.Webhook,
    as: :edit_message

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.execute_slack/3` directly instead.
  """
  defdelegate execute_slack_webhook(webhook_id, webhook_token, wait \\ false),
    to: Nostrum.Api.Webhook,
    as: :execute_slack

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Webhook.execute_git/3` directly instead.
  """
  defdelegate execute_git_webhook(webhook_id, webhook_token, wait \\ false),
    to: Nostrum.Api.Webhook,
    as: :execute_git

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Self.application_information/0` directly instead.
  """
  defdelegate get_application_information,
    to: Nostrum.Api.Self,
    as: :application_information

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.global_commands/1` directly instead.
  """
  defdelegate get_global_application_commands(application_id \\ Me.get().id),
    to: Nostrum.Api.ApplicationCommand,
    as: :global_commands

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.create_global_command/2` directly instead.
  """
  defdelegate create_global_application_command(application_id \\ Me.get().id, command),
    to: Nostrum.Api.ApplicationCommand,
    as: :create_global_command

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.edit_global_command/3` directly instead.
  """
  defdelegate edit_global_application_command(application_id \\ Me.get().id, command_id, command),
    to: Nostrum.Api.ApplicationCommand,
    as: :edit_global_command

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.delete_global_command/2` directly instead.
  """
  defdelegate delete_global_application_command(application_id \\ Me.get().id, command_id),
    to: Nostrum.Api.ApplicationCommand,
    as: :delete_global_command

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.bulk_overwrite_global_commands/2` directly instead.
  """
  defdelegate bulk_overwrite_global_application_commands(application_id \\ Me.get().id, commands),
    to: Nostrum.Api.ApplicationCommand,
    as: :bulk_overwrite_global_commands

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.guild_commands/2` directly instead.
  """
  defdelegate get_guild_application_commands(application_id \\ Me.get().id, guild_id),
    to: Nostrum.Api.ApplicationCommand,
    as: :guild_commands

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.create_guild_command/3` directly instead.
  """
  defdelegate create_guild_application_command(application_id \\ Me.get().id, guild_id, command),
    to: Nostrum.Api.ApplicationCommand,
    as: :create_guild_command

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.edit_guild_command/4` directly instead.
  """
  defdelegate edit_guild_application_command(
                application_id \\ Me.get().id,
                guild_id,
                command_id,
                command
              ),
              to: Nostrum.Api.ApplicationCommand,
              as: :edit_guild_command

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.delete_guild_command/3` directly instead.
  """
  defdelegate delete_guild_application_command(
                application_id \\ Me.get().id,
                guild_id,
                command_id
              ),
              to: Nostrum.Api.ApplicationCommand,
              as: :delete_guild_command

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.bulk_overwrite_guild_commands/3` directly instead.
  """
  defdelegate bulk_overwrite_guild_application_commands(
                application_id \\ Me.get().id,
                guild_id,
                commands
              ),
              to: Nostrum.Api.ApplicationCommand,
              as: :bulk_overwrite_guild_commands

  # Why the two separate functions here?
  # For the standard use case of "responding to an interaction retrieved
  # from the gateway", `create_interaction_response/2` is perfectly
  # sufficient. However, when one, for instance, uses Nostrum in a web
  # service, or wants to respond to interactions at a later point in time,
  # we do not want the user to manually have to reconstruct interactions.
  @doc """
  Same as `create_interaction_response/3`, but directly takes the
  `t:Nostrum.Struct.Interaction.t/0` received from the gateway.
  """
  @spec create_interaction_response(Interaction.t(), map()) :: {:ok} | error
  def create_interaction_response(interaction, response) do
    create_interaction_response(interaction.id, interaction.token, response)
  end

  @doc """
  Same as `create_interaction_response/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_interaction_response!(Interaction.t(), map()) :: no_return() | {:ok}
  def create_interaction_response!(interaction, response) do
    create_interaction_response!(interaction.id, interaction.token, response)
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Interaction.create_response/3` directly instead.
  """
  defdelegate create_interaction_response(id, token, response),
    to: Nostrum.Api.Interaction,
    as: :create_response

  @deprecated "Bang functions will be removed in v1.0"
  def create_interaction_response!(id, token, response) do
    create_interaction_response(id, token, response)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Interaction.original_response/2` directly instead.
  """
  defdelegate get_original_interaction_response(interaction),
    to: Nostrum.Api.Interaction,
    as: :original_response

  @doc """
  Same as `edit_interaction_response/3`, but directly takes the
  `t:Nostrum.Struct.Interaction.t/0` received from the gateway.
  """
  @doc since: "0.5.0"
  @spec edit_interaction_response(Interaction.t(), map()) :: {:ok, Message.t()} | error
  def edit_interaction_response(%Interaction{} = interaction, response) do
    edit_interaction_response(interaction.application_id, interaction.token, response)
  end

  @doc """
  Same as `edit_interaction_response/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec edit_interaction_response!(Interaction.t(), map()) :: no_return() | Message.t()
  def edit_interaction_response!(%Interaction{} = interaction, response) do
    edit_interaction_response!(interaction.application_id, interaction.token, response)
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Interaction.edit_response/3` directly instead.
  """
  defdelegate edit_interaction_response(id \\ Me.get().id, token, response),
    to: Nostrum.Api.Interaction,
    as: :edit_response

  @doc """
  Same as `edit_interaction_response/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec edit_interaction_response!(User.id(), Interaction.token(), map()) ::
          no_return() | Message.t()
  def edit_interaction_response!(id \\ Me.get().id, token, response) do
    edit_interaction_response(id, token, response)
    |> bangify
  end

  @doc """
  Same as `delete_interaction_response/3`, but directly takes the
  `t:Nostrum.Struct.Interaction.t/0` received from the gateway.
  """
  @doc since: "0.5.0"
  @spec delete_interaction_response(Interaction.t()) :: {:ok} | error
  def delete_interaction_response(%Interaction{} = interaction) do
    delete_interaction_response(interaction.application_id, interaction.token)
  end

  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_interaction_response!(Interaction.t()) :: no_return() | {:ok}
  def delete_interaction_response!(%Interaction{} = interaction) do
    delete_interaction_response(interaction.application_id, interaction.token)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Interaction.delete_response/2` directly instead.
  """
  defdelegate delete_interaction_response(id \\ Me.get().id, token),
    to: Nostrum.Api.Interaction,
    as: :delete_response

  @doc """
  Same as `delete_interaction_response/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_interaction_response!(User.id(), Interaction.token()) :: no_return() | {:ok}
  def delete_interaction_response!(id \\ Me.get().id, token) do
    delete_interaction_response(id, token)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Interaction.create_followup_message/3` directly instead.
  """
  defdelegate create_followup_message(application_id \\ Me.get().id, token, webhook_payload),
    to: Nostrum.Api.Interaction,
    as: :create_followup_message

  @doc """
  Same as `create_followup_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec create_followup_message!(User.id(), Interaction.token(), map()) ::
          no_return() | Message.t()
  def create_followup_message!(application_id \\ Me.get().id, token, webhook_payload) do
    create_followup_message(application_id, token, webhook_payload)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Interaction.delete_followup_message/3` directly instead.
  """
  defdelegate delete_interaction_followup_message(
                application_id \\ Me.get().id,
                token,
                message_id
              ),
              to: Nostrum.Api.Interaction,
              as: :delete_followup_message

  @doc """
  Same as `delete_interaction_followup_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @deprecated "Bang functions will be removed in v1.0"
  @spec delete_interaction_followup_message!(User.id(), Interaction.token(), Message.id()) ::
          no_return() | {:ok}
  def delete_interaction_followup_message!(
        application_id \\ Me.get().id,
        token,
        message_id
      ) do
    delete_interaction_followup_message(application_id, token, message_id)
    |> bangify
  end

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.guild_permissions/2` directly instead.
  """
  defdelegate get_guild_application_command_permissions(application_id \\ Me.get().id, guild_id),
    to: Nostrum.Api.ApplicationCommand,
    as: :guild_permissions

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.permissions/3` directly instead.
  """
  defdelegate get_application_command_permissions(
                application_id \\ Me.get().id,
                guild_id,
                command_id
              ),
              to: Nostrum.Api.ApplicationCommand,
              as: :permissions

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.edit_command_permissions/4` directly instead.
  """
  defdelegate edit_application_command_permissions(
                application_id \\ Me.get().id,
                guild_id,
                command_id,
                permissions
              ),
              to: Nostrum.Api.ApplicationCommand,
              as: :edit_command_permissions

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.ApplicationCommand.batch_edit_permissions/3` directly instead.
  """
  defdelegate batch_edit_application_command_permissions(
                application_id \\ Me.get().id,
                guild_id,
                permissions
              ),
              to: Nostrum.Api.ApplicationCommand,
              as: :batch_edit_permissions

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.create_with_message/4` directly instead.
  """
  defdelegate start_thread_with_message(channel_id, message_id, options, reason \\ nil),
    to: Nostrum.Api.Thread,
    as: :create_with_message

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.create_in_forum/3` directly instead.
  """
  defdelegate start_thread_in_forum_channel(channel_id, options, reason \\ nil),
    to: Nostrum.Api.Thread,
    as: :create_in_forum

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.member/2` directly instead.
  """
  defdelegate get_thread_member(thread_id, user_id),
    to: Nostrum.Api.Thread,
    as: :member

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.members/1` directly instead.
  """
  defdelegate get_thread_members(thread_id),
    to: Nostrum.Api.Thread,
    as: :members

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.list/1` directly instead.
  """
  defdelegate list_guild_threads(guild_id),
    to: Nostrum.Api.Thread,
    as: :list

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.public_archived_threads/2` directly instead.
  """
  defdelegate list_public_archived_threads(channel_id, options \\ []),
    to: Nostrum.Api.Thread,
    as: :public_archived_threads

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.private_archived_threads/2` directly instead.
  """
  defdelegate list_private_archived_threads(channel_id, options \\ []),
    to: Nostrum.Api.Thread,
    as: :private_archived_threads

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.joined_private_archived_threads/2` directly instead.
  """
  defdelegate list_joined_private_archived_threads(channel_id, options \\ []),
    to: Nostrum.Api.Thread,
    as: :joined_private_archived_threads

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.join/1` directly instead.
  """
  defdelegate join_thread(thread_id),
    to: Nostrum.Api.Thread,
    as: :join

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.add_member/2` directly instead.
  """
  defdelegate add_thread_member(thread_id, user_id),
    to: Nostrum.Api.Thread,
    as: :add_member

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.leave/1` directly instead.
  """
  defdelegate leave_thread(thread_id),
    to: Nostrum.Api.Thread,
    as: :leave

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.Thread.remove_member/2` directly instead.
  """
  defdelegate remove_thread_member(thread_id, user_id),
    to: Nostrum.Api.Thread,
    as: :remove_member

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.AutoModeration.rules/1` directly instead.
  """
  defdelegate get_guild_auto_moderation_rules(guild_id),
    to: Nostrum.Api.AutoModeration,
    as: :rules

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.AutoModeration.rule/2` directly instead.
  """
  defdelegate get_guild_auto_moderation_rule(guild_id, rule_id),
    to: Nostrum.Api.AutoModeration,
    as: :rule

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.AutoModeration.create_rule/2` directly instead.
  """
  defdelegate create_guild_auto_moderation_rule(guild_id, options),
    to: Nostrum.Api.AutoModeration,
    as: :create_rule

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.AutoModeration.modify_rule/3` directly instead.
  """
  defdelegate modify_guild_auto_moderation_rule(guild_id, rule_id, options),
    to: Nostrum.Api.AutoModeration,
    as: :modify_rule

  @deprecated """
  Calling `Nostrum.Api` functions directly will be removed in v1.0
  Use `Nostrum.Api.AutoModeration.delete_rule/2` directly instead.
  """
  defdelegate delete_guild_auto_moderation_rule(guild_id, rule_id),
    to: Nostrum.Api.AutoModeration,
    as: :delete_rule

  @spec request(map()) :: {:ok} | {:ok, String.t()} | error
  def request(request) do
    Ratelimiter.queue(request)
  end

  @spec request(atom(), String.t(), any, keyword() | map()) ::
          {:ok} | {:ok, String.t()} | error
  def request(method, route, body \\ "", params \\ [])

  def request(method, route, %{} = body, params) when has_files(body),
    do: request_multipart(method, route, body, params)

  def request(method, route, %{data: data} = body, params) when has_files(data),
    do: request_multipart(method, route, body, params)

  def request(method, route, body, params) do
    %{
      method: method,
      route: route,
      body: body,
      params: params,
      headers: [{"content-type", "application/json"}]
    }
    |> request()
  end

  @spec request_multipart(atom(), String.t(), any, keyword() | map()) ::
          {:ok} | {:ok, String.t()} | error
  def request_multipart(method, route, body, params \\ []) do
    boundary = Helpers.generate_boundary()

    {files, body} =
      Helpers.combine_files(body)
      |> Helpers.pop_files()

    json = Jason.encode_to_iodata!(body)

    %{
      method: method,
      route: route,
      # Hello :gun test suite :^)
      body: {:multipart, create_multipart(files, json, boundary)},
      params: params,
      headers: [
        {"content-type", "multipart/form-data; boundary=#{boundary}"}
      ]
    }
    |> request()
  end

  @doc false
  def bangify({:error, error}), do: raise(error)
  def bangify({:ok, body}), do: body
  def bangify({:ok}), do: {:ok}

  def create_multipart(files, json, boundary) do
    json_mime = MIME.type("json")
    json_size = :erlang.iolist_size(json)

    file_parts =
      files
      |> Enum.with_index(0)
      |> Enum.map(fn {f, i} -> create_file_part_for_multipart(f, i, boundary) end)

    [
      ~s|--#{boundary}#{@crlf}|,
      file_parts
      | [
          ~s|content-length: #{json_size}#{@crlf}|,
          ~s|content-type: #{json_mime}#{@crlf}|,
          ~s|content-disposition: form-data; name="payload_json"#{@crlf}#{@crlf}|,
          json,
          ~s|#{@crlf}--#{boundary}--#{@crlf}|
        ]
    ]
  end

  def create_file_part_for_multipart(file, index, boundary, name_override \\ nil) do
    {body, name} = get_file_contents(file)

    file_mime = MIME.from_path(name)
    file_size = :erlang.iolist_size(body)

    field_name =
      if name_override do
        name_override
      else
        "files[#{index}]"
      end

    [
      ~s|content-length: #{file_size}#{@crlf}|,
      ~s|content-type: #{file_mime}#{@crlf}|,
      ~s|content-disposition: form-data; name="#{field_name}"; filename="#{name}"#{@crlf}#{@crlf}|,
      body,
      ~s|#{@crlf}--#{boundary}#{@crlf}|
    ]
  end

  defp get_file_contents(path) when is_binary(path) do
    {File.read!(path), Path.basename(path)}
  end

  defp get_file_contents(%{body: body, name: name}), do: {body, name}
end
