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
    Nostrum.Api.get_channel_messages(12345678912345, :infinity, {})
  end
  messages = Task.await t

  # A lot of times we don't care about the return value of the function
  Task.start fn ->
    messages = ["in", "the", "end", "it", "doesn't", "even", "matter"]
    Enum.each messages, &Nostrum.Api.create_message!(12345678912345, &1)
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
  messages = Nostrum.Api.get_pinned_messages!(12345678912345)

  authors =
    Enum.map messages, fn msg ->
      author_id = String.to_integer(msg.author.id)
      Nostrum.Cache.User.get!(id: author_id)
    end
  ```
  """

  @crlf "\r\n"

  require Logger

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  alias Nostrum.Api.Ratelimiter
  alias Nostrum.Cache.Me
  alias Nostrum.{Constants, Snowflake, Util}

  alias Nostrum.Struct.{
    ApplicationCommand,
    AutoModerationRule,
    Channel,
    Embed,
    Emoji,
    Guild,
    Interaction,
    Invite,
    Message,
    Message.Poll,
    Sticker,
    ThreadMember,
    User,
    Webhook
  }

  alias Nostrum.Struct.Guild.{AuditLog, AuditLogEntry, Member, Role, ScheduledEvent}

  defguard has_files(args) when is_map_key(args, :files) or is_map_key(args, :file)

  def handle_request_with_decode(response)
  def handle_request_with_decode({:ok, body}), do: {:ok, Jason.decode!(body, keys: :atoms)}
  def handle_request_with_decode({:error, _} = error), do: error

  def handle_request_with_decode(response, type)
  # add_guild_member/3 can return both a 201 and a 204
  def handle_request_with_decode({:ok}, _type), do: {:ok}
  def handle_request_with_decode({:error, _} = error, _type), do: error

  def handle_request_with_decode({:ok, body}, type) do
    convert =
      body
      |> Jason.decode!(keys: :atoms)
      |> Util.cast(type)

    {:ok, convert}
  end

  def handle_request_with_decode!(response)
  def handle_request_with_decode!({:ok, body}), do: Jason.decode!(body, keys: :atoms)
  def handle_request_with_decode!({:error, error}), do: raise(error)

  def handle_request_with_decode!(response, type)
  # add_guild_member/3 can return both a 201 and a 204
  def handle_request_with_decode!({:ok}, _type), do: {:ok}
  def handle_request_with_decode!({:error, error}, _type), do: raise(error)

  def handle_request_with_decode!({:ok, body}, type) do
    body
    |> Jason.decode!(keys: :atoms)
    |> Util.cast(type)
  end

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

  @doc """
  Updates the status of the bot for a certain shard.

  ## Parameters
    - `pid` - Pid of the shard.
    - `status` - Status of the bot.
    - `game` - The 'playing' text of the bot. Empty will clear.
    - `type` - The type of status to show. 0 (Playing) | 1 (Streaming) | 2 (Listening) | 3 (Watching)
    - `stream` - URL of twitch.tv stream
  """
  @spec update_shard_status(pid, status, String.t(), integer, String.t() | nil) :: :ok
  def update_shard_status(pid, status, game, type \\ 0, stream \\ nil) do
    Nostrum.Api.Self.update_shard_status(pid, status, game, type, stream)
  end

  @doc """
  Updates the status of the bot for all shards.

  See `update_shard_status/5` for usage.
  """
  @spec update_status(status, String.t(), integer, String.t() | nil) :: :ok
  def update_status(status, game, type \\ 0, stream \\ nil) do
    Nostrum.Api.Self.update_status(status, game, type, stream)
  end

  @doc """
  Joins, moves, or disconnects the bot from a voice channel.

  The correct shard to send the update to will be inferred from the
  `guild_id`. If a corresponding `guild_id` is not found a cache error will be
  raised.

  To disconnect from a channel, `channel_id` should be set to `nil`.
  """
  @spec update_voice_state(Guild.id(), Channel.id() | nil, boolean, boolean) :: no_return | :ok
  def update_voice_state(guild_id, channel_id, self_mute \\ false, self_deaf \\ false) do
    Nostrum.Api.Self.update_voice_state(guild_id, channel_id, self_mute, self_deaf)
  end

  @doc ~S"""
  Posts a message to a guild text or DM channel.

  This endpoint requires the `VIEW_CHANNEL` and `SEND_MESSAGES` permissions. It
  may situationally need the `SEND_MESSAGES_TTS` permission. It fires the
  `t:Nostrum.Consumer.message_create/0` event.

  If `options` is a string, `options` will be used as the message's content.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:content` (string) - the message contents (up to 2000 characters)
    * `:nonce` (`t:Nostrum.Snowflake.t/0`) - a nonce that can be used for
    optimistic message sending
    * `:tts` (boolean) - true if this is a TTS message
    * `:file` (`t:Path.t/0` | map) - the path of the file being sent, or a map with the following keys
    if sending a binary from memory
      * `:name` (string) - the name of the file
      * `:body` (string) - binary you wish to send
    * `:files` - a list of files where each element is the same format as the `:file` option. If both
    `:file` and `:files` are specified, `:file` will be prepended to the `:files` list.
    * `:embeds` (`t:Nostrum.Struct.Embed.t/0`) - a list of embedded rich content
    * `:allowed_mentions` (`t:allowed_mentions/0`) - see the allowed mentions type documentation
    * `:message_reference` (`map`) - See "Message references" below
    * `:poll` (`t:Nostrum.Struct.Message.Poll.t/0`) - A poll object to send with the message

    At least one of the following is required: `:content`, `:file`, `:embeds`, `:poll`.

  ### Message reference

  You can create a reply to another message on guilds using this option, given
  that you have the ``VIEW_MESSAGE_HISTORY`` permission. To do so, include the
  ``message_reference`` field in your call. The complete structure
  documentation can be found [on the Discord Developer
  Portal](https://discord.com/developers/docs/resources/channel#message-object-message-reference-structure),
  but simply passing ``message_id`` will suffice:

  ```elixir
  def my_command(msg) do
    # Reply to the author - ``msg`` is a ``Nostrum.Struct.Message``
    Nostrum.Api.create_message(
      msg.channel_id,
      content: "Hello",
      message_reference: %{message_id: msg.id}
    )
  end
  ```

  Passing a list will merge the settings provided

  ## Examples

  ```elixir
  Nostrum.Api.create_message(43189401384091, content: "hello world!")

  Nostrum.Api.create_message(43189401384091, "hello world!")

  import Nostrum.Struct.Embed
  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("embed")
    |> put_description("new desc")
  Nostrum.Api.create_message(43189401384091, embeds: [embed])

  Nostrum.Api.create_message(43189401384091, file: "/path/to/file.txt")

  Nostrum.Api.create_message(43189401384091, content: "hello world!", embeds: [embed], file: "/path/to/file.txt")

  Nostrum.Api.create_message(43189401384091, content: "Hello @everyone", allowed_mentions: :none)
  ```
  """
  @spec create_message(Channel.id() | Message.t(), options | String.t()) ::
          error | {:ok, Message.t()}
  def create_message(channel_id, options) do
    Nostrum.Api.Message.create(channel_id, options)
  end

  @doc ~S"""
  Same as `create_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_message!(Channel.id() | Message.t(), options | String.t()) ::
          no_return | Message.t()
  def create_message!(channel_id, options) do
    create_message(channel_id, options)
    |> bangify
  end

  @doc ~S"""
  Edits a previously sent message in a channel.

  This endpoint requires the `VIEW_CHANNEL` permission. It fires the
  `t:Nostrum.Consumer.message_update/0` event.

  If `options` is a string, `options` will be used as the message's content.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

  * `:content` (string) - the message contents (up to 2000 characters)
  * `:embeds` (`t:Nostrum.Struct.Embed.t/0`) - a list of embedded rich content
  * `:files` - a list of files where each element is the same format as the
  `:file` option. If both `:file` and `:files` are specified, `:file` will be
  prepended to the `:files` list. See `create_message/2` for more information.

  Note that if you edit a message with attachments, all attachments that should
  be present after edit **must** be included in your request body. This
  includes attachments that were sent in the original request.

  ## Examples

  ```elixir
  Nostrum.Api.edit_message(43189401384091, 1894013840914098, content: "hello world!")

  Nostrum.Api.edit_message(43189401384091, 1894013840914098, "hello world!")

  import Nostrum.Struct.Embed
  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("embed")
    |> put_description("new desc")
  Nostrum.Api.edit_message(43189401384091, 1894013840914098, embeds: [embed])

  Nostrum.Api.edit_message(43189401384091, 1894013840914098, content: "hello world!", embeds: [embed])
  ```
  """
  @spec edit_message(Channel.id(), Message.id(), options | String.t()) ::
          error | {:ok, Message.t()}
  def edit_message(channel_id, message_id, options) do
    Nostrum.Api.Message.edit(channel_id, message_id, options)
  end

  @doc ~S"""
  Same as `edit_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
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

  @doc ~S"""
  Deletes a message.

  This endpoint requires the 'VIEW_CHANNEL' and 'MANAGE_MESSAGES' permission. It
  fires the `MESSAGE_DELETE` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.delete_message(43189401384091, 43189401384091)
  ```
  """
  @spec delete_message(Channel.id(), Message.id()) :: error | {:ok}
  def delete_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Nostrum.Api.Message.delete(channel_id, message_id)
  end

  @doc ~S"""
  Same as `delete_message/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_message!(Message.t()) :: error | {:ok}
  def delete_message!(%Message{id: id, channel_id: c_id}) do
    delete_message(c_id, id)
    |> bangify
  end

  @doc ~S"""
  Same as `delete_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_message!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_message!(channel_id, message_id) do
    delete_message(channel_id, message_id)
    |> bangify
  end

  @doc ~S"""
  Creates a reaction for a message.

  This endpoint requires the `VIEW_CHANNEL` and `READ_MESSAGE_HISTORY`
  permissions. Additionally, if nobody else has reacted to the message with
  the `emoji`, this endpoint requires the `ADD_REACTIONS` permission. It
  fires a `t:Nostrum.Consumer.message_reaction_add/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  # Using a Nostrum.Struct.Emoji.
  emoji = %Nostrum.Struct.Emoji{id: 43819043108, name: "foxbot"}
  Nostrum.Api.create_reaction(123123123123, 321321321321, emoji)

  # Using a base 16 emoji string.
  Nostrum.Api.create_reaction(123123123123, 321321321321, "\xF0\x9F\x98\x81")

  ```

  For other emoji string examples, see `t:Nostrum.Struct.Emoji.api_name/0`.
  """
  @spec create_reaction(Channel.id(), Message.id(), emoji) :: error | {:ok}
  def create_reaction(channel_id, message_id, emoji) do
    Nostrum.Api.Message.react(channel_id, message_id, emoji)
  end

  @doc ~S"""
  Same as `create_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_reaction!(Channel.id(), Message.id(), emoji) :: no_return | {:ok}
  def create_reaction!(channel_id, message_id, emoji) do
    create_reaction(channel_id, message_id, emoji)
    |> bangify
  end

  @doc ~S"""
  Deletes a reaction the current user has made for the message.

  This endpoint requires the `VIEW_CHANNEL` and `READ_MESSAGE_HISTORY`
  permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `create_reaction/3` for similar examples.
  """
  @spec delete_own_reaction(Channel.id(), Message.id(), emoji) :: error | {:ok}
  def delete_own_reaction(channel_id, message_id, emoji)

  def delete_own_reaction(channel_id, message_id, %Emoji{} = emoji),
    do: delete_own_reaction(channel_id, message_id, Emoji.api_name(emoji))

  def delete_own_reaction(channel_id, message_id, emoji_api_name) do
    Nostrum.Api.Message.unreact(channel_id, message_id, emoji_api_name)
  end

  @doc ~S"""
  Same as `delete_own_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_own_reaction!(Channel.id(), Message.id(), emoji) :: no_return | {:ok}
  def delete_own_reaction!(channel_id, message_id, emoji) do
    delete_own_reaction(channel_id, message_id, emoji)
    |> bangify
  end

  @doc ~S"""
  Deletes another user's reaction from a message.

  This endpoint requires the `VIEW_CHANNEL`, `READ_MESSAGE_HISTORY`, and
  `MANAGE_MESSAGES` permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `create_reaction/3` for similar examples.
  """
  @spec delete_user_reaction(Channel.id(), Message.id(), emoji, User.id()) :: error | {:ok}
  def delete_user_reaction(channel_id, message_id, emoji, user_id)

  def delete_user_reaction(channel_id, message_id, %Emoji{} = emoji, user_id),
    do: delete_user_reaction(channel_id, message_id, Emoji.api_name(emoji), user_id)

  def delete_user_reaction(channel_id, message_id, emoji_api_name, user_id) do
    Nostrum.Api.Message.delete_user_reaction(channel_id, message_id, emoji_api_name, user_id)
  end

  @doc ~S"""
  Same as `delete_user_reaction/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_user_reaction!(Channel.id(), Message.id(), emoji, User.id()) :: no_return | {:ok}
  def delete_user_reaction!(channel_id, message_id, emoji, user_id) do
    delete_user_reaction(channel_id, message_id, emoji, user_id)
    |> bangify
  end

  @doc ~S"""
  Deletes all reactions of a given emoji from a message.

  This endpoint requires the `MANAGE_MESSAGES` permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove_emoji/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `create_reaction/3` for similar examples.
  """
  @spec delete_reaction(Channel.id(), Message.id(), emoji) :: error | {:ok}
  def delete_reaction(channel_id, message_id, emoji)

  def delete_reaction(channel_id, message_id, %Emoji{} = emoji),
    do: delete_reaction(channel_id, message_id, Emoji.api_name(emoji))

  def delete_reaction(channel_id, message_id, emoji_api_name) do
    Nostrum.Api.Message.delete_emoji_reactions(channel_id, message_id, emoji_api_name)
  end

  @doc ~S"""
  Same as `delete_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_reaction!(Channel.id(), Message.id(), emoji) :: no_return | {:ok}
  def delete_reaction!(channel_id, message_id, emoji) do
    delete_reaction(channel_id, message_id, emoji)
    |> bangify
  end

  @doc ~S"""
  Gets all users who reacted with an emoji.

  This endpoint requires the `VIEW_CHANNEL` and `READ_MESSAGE_HISTORY` permissions.

  If successful, returns `{:ok, users}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  The optional `params` are `after`, the user ID to query after, absent by default,
  and `limit`, the max number of users to return, 1-100, 25 by default.

  See `create_reaction/3` for similar examples.
  """
  @spec get_reactions(Channel.id(), Message.id(), emoji, keyword()) :: error | {:ok, [User.t()]}
  def get_reactions(channel_id, message_id, emoji, params \\ [])

  def get_reactions(channel_id, message_id, %Emoji{} = emoji, params),
    do: get_reactions(channel_id, message_id, Emoji.api_name(emoji), params)

  def get_reactions(channel_id, message_id, emoji_api_name, params) do
    Nostrum.Api.Message.reactions(channel_id, message_id, emoji_api_name, params)
  end

  @doc ~S"""
  Same as `get_reactions/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_reactions!(Channel.id(), Message.id(), emoji, keyword()) :: no_return | [User.t()]
  def get_reactions!(channel_id, message_id, emoji, params \\ []) do
    get_reactions(channel_id, message_id, emoji, params)
    |> bangify
  end

  @doc ~S"""
  Deletes all reactions from a message.

  This endpoint requires the `VIEW_CHANNEL`, `READ_MESSAGE_HISTORY`, and
  `MANAGE_MESSAGES` permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove_all/0` event.

  If successful, returns `{:ok}`. Otherwise, return `t:Nostrum.Api.error/0`.
  """
  @spec delete_all_reactions(Channel.id(), Message.id()) :: error | {:ok}
  def delete_all_reactions(channel_id, message_id) do
    Nostrum.Api.Message.clear_reactions(channel_id, message_id)
  end

  @doc ~S"""
  Same as `delete_all_reactions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_all_reactions!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_all_reactions!(channel_id, message_id) do
    delete_all_reactions(channel_id, message_id)
    |> bangify
  end

  @doc ~S"""
  Get voters for the provided answer on the poll attached to the provided message.

  If successful, returns `{:ok, users}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  The optional `params` are `after`, the user ID to query after, absent by default,
  and `limit`, the max number of users to return, 1-100, 25 by default. Results are
  sorted by Discord user snowflake (ID) in ascending order.
  """
  @spec get_poll_answer_voters(Channel.id(), Message.id(), Poll.Answer.answer_id()) ::
          error | {:ok, [User.t()]}
  def get_poll_answer_voters(channel_id, message_id, answer_id, params \\ []) do
    Nostrum.Api.Poll.answer_voters(channel_id, message_id, answer_id, params)
  end

  @doc ~S"""
  Same as `get_poll_answer_voters/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_poll_answer_voters!(Channel.id(), Message.id(), Poll.Answer.answer_id()) :: [User.t()]
  def get_poll_answer_voters!(channel_id, message_id, answer_id, params \\ []) do
    get_poll_answer_voters(channel_id, message_id, answer_id, params)
    |> bangify
  end

  @doc ~S"""
  Expire (close voting on) a poll before the scheduled end time.

  Returns the original message containing the poll.
  """
  @spec expire_poll(Channel.id(), Message.id()) :: error | {:ok, Message.t()}
  def expire_poll(channel_id, message_id) do
    Nostrum.Api.Poll.expire(channel_id, message_id)
  end

  @doc ~S"""
  Same as `expire_poll/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec expire_poll!(Channel.id(), Message.id()) :: Message.t()
  def expire_poll!(channel_id, message_id) do
    expire_poll(channel_id, message_id)
    |> bangify
  end

  @doc ~S"""
  Gets a channel.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_channel(381889573426429952)
  {:ok, %Nostrum.Struct.Channel{id: 381889573426429952}}
  ```
  """
  @spec get_channel(Channel.id()) :: error | {:ok, Channel.t()}
  def get_channel(channel_id) when is_snowflake(channel_id) do
    Nostrum.Api.Channel.get(channel_id)
  end

  @doc ~S"""
  Same as `get_channel/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_channel!(Channel.id()) :: no_return | Channel.t()
  def get_channel!(channel_id) do
    get_channel(channel_id)
    |> bangify
  end

  @doc ~S"""
  Modifies a channel's settings.

  An optional `reason` can be given for the guild audit log.

  If a `t:Nostrum.Struct.Channel.guild_channel/0` is being modified, this
  endpoint requires the `MANAGE_CHANNEL` permission. It fires a
  `t:Nostrum.Consumer.channel_update/0` event. If a
  `t:Nostrum.Struct.Channel.guild_category_channel/0` is being modified, then this
  endpoint fires multiple `t:Nostrum.Consumer.channel_update/0` events.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - 2-100 character channel name
    * `:position` (integer) - the position of the channel in the left-hand listing
    * `:topic` (string) (`t:Nostrum.Struct.Channel.text_channel/0` only) -
    0-1024 character channel topic
    * `:nsfw` (boolean) (`t:Nostrum.Struct.Channel.text_channel/0` only) -
    if the channel is nsfw
    * `:bitrate` (integer) (`t:Nostrum.Struct.Channel.voice_channel/0` only) -
    the bitrate (in bits) of the voice channel; 8000 to 96000 (128000 for VIP servers)
    * `:user_limit` (integer) (`t:Nostrum.Struct.Channel.voice_channel/0` only) -
    the user limit of the voice channel; 0 refers to no limit, 1 to 99 refers to a user limit
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0` or equivalent map) -
    channel or category-specific permissions
    * `:parent_id` (`t:Nostrum.Struct.Channel.id/0`) (`t:Nostrum.Struct.Channel.guild_channel/0` only) -
    id of the new parent category for a channel

  ## Examples

  ```elixir
  Nostrum.Api.modify_channel(41771983423143933, name: "elixir-nostrum", topic: "nostrum discussion")
  {:ok, %Nostrum.Struct.Channel{id: 41771983423143933, name: "elixir-nostrum", topic: "nostrum discussion"}}

  Nostrum.Api.modify_channel(41771983423143933)
  {:ok, %Nostrum.Struct.Channel{id: 41771983423143933}}
  ```
  """
  @spec modify_channel(Channel.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Channel.t()}
  def modify_channel(channel_id, options, reason \\ nil) do
    Nostrum.Api.Channel.modify(channel_id, options, reason)
  end

  @doc ~S"""
  Same as `modify_channel/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_channel!(Channel.id(), options, AuditLogEntry.reason()) :: no_return | Channel.t()
  def modify_channel!(channel_id, options, reason \\ nil) do
    modify_channel(channel_id, options, reason)
    |> bangify
  end

  @doc ~S"""
  Deletes a channel.

  An optional `reason` can be provided for the guild audit log.

  If deleting a `t:Nostrum.Struct.Channel.guild_channel/0`, this endpoint requires
  the `MANAGE_CHANNELS` permission. It fires a
  `t:Nostrum.Consumer.channel_delete/0`. If a `t:Nostrum.Struct.Channel.guild_category_channel/0`
  is deleted, then a `t:Nostrum.Consumer.channel_update/0` event will fire
  for each channel under the category.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.delete_channel(421533712753360896)
  {:ok, %Nostrum.Struct.Channel{id: 421533712753360896}}
  ```
  """
  @spec delete_channel(Channel.id(), AuditLogEntry.reason()) :: error | {:ok, Channel.t()}
  def delete_channel(channel_id, reason \\ nil) when is_snowflake(channel_id) do
    Nostrum.Api.Channel.delete(channel_id, reason)
  end

  @doc ~S"""
  Same as `delete_channel/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_channel!(Channel.id(), AuditLogEntry.reason()) :: no_return | Channel.t()
  def delete_channel!(channel_id, reason \\ nil) do
    delete_channel(channel_id, reason)
    |> bangify
  end

  @doc ~S"""
  Retrieves a channel's messages around a `locator` up to a `limit`.

  This endpoint requires the 'VIEW_CHANNEL' permission. If the current user
  is missing the 'READ_MESSAGE_HISTORY' permission, then this function will
  return no messages.

  If successful, returns `{:ok, messages}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_channel_messages(43189401384091, 5, {:before, 130230401384})
  ```
  """
  @spec get_channel_messages(Channel.id(), limit, locator) :: error | {:ok, [Message.t()]}
  def get_channel_messages(channel_id, limit, locator \\ {}) when is_snowflake(channel_id) do
    Nostrum.Api.Channel.messages(channel_id, limit, locator)
  end

  @doc ~S"""
  Same as `get_channel_messages/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_channel_messages!(Channel.id(), limit, locator) :: no_return | [Message.t()]
  def get_channel_messages!(channel_id, limit, locator \\ {}) do
    get_channel_messages(channel_id, limit, locator)
    |> bangify
  end

  @doc ~S"""
  Retrieves a message from a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'READ_MESSAGE_HISTORY' permissions.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_channel_message(43189401384091, 198238475613443)
  ```
  """
  @spec get_channel_message(Channel.id(), Message.id()) :: error | {:ok, Message.t()}
  def get_channel_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Nostrum.Api.Message.get(channel_id, message_id)
  end

  @doc ~S"""
  Same as `get_channel_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_channel_message!(Channel.id(), Message.id()) :: no_return | Message.t()
  def get_channel_message!(channel_id, message_id) do
    get_channel_message(channel_id, message_id)
    |> bangify
  end

  @doc """
  Deletes multiple messages from a channel.

  `messages` is a list of `Nostrum.Struct.Message.id` that you wish to delete.
  When given more than 100 messages, this function will chunk the given message
  list into blocks of 100 and send them off to the API. It will stop deleting
  on the first error that occurs. Keep in mind that deleting thousands of
  messages will take a pretty long time and it may be proper to just delete
  the channel you want to bulk delete in and recreate it.

  This method can only delete messages sent within the last two weeks.
  `Filter` is an optional parameter that specifies whether messages sent over
  two weeks ago should be filtered out; defaults to `true`.
  """
  @spec bulk_delete_messages(integer, [Nostrum.Struct.Message.id()], boolean) :: error | {:ok}
  def bulk_delete_messages(channel_id, messages, filter) do
    Nostrum.Api.Channel.bulk_delete_messages(channel_id, messages, filter)
  end

  @doc """
  Same as `bulk_delete_messages/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec bulk_delete_messages!(integer, [Nostrum.Struct.Message.id()], boolean) ::
          no_return | {:ok}
  def bulk_delete_messages!(channel_id, messages, filter \\ true) do
    bulk_delete_messages(channel_id, messages, filter)
    |> bangify
  end

  @doc """
  Edit the permission overwrites for a user or role.

  Role or user to overwrite is specified by `overwrite_id`.

  `permission_info` is a map with the following keys:
   * `type` - Required; `member` if editing a user, `role` if editing a role.
   * `allow` - Bitwise value of allowed permissions.
   * `deny` - Bitwise value of denied permissions.
   * `type` - `member` if editing a user, `role` if editing a role.

  An optional `reason` can be provided for the audit log.

   `allow` and `deny` are defaulted to `0`, meaning that even if you don't
   specify them, they will override their respective former values in an
   existing overwrite.
  """
  @spec edit_channel_permissions(
          integer,
          integer,
          %{
            required(:type) => String.t(),
            optional(:allow) => integer,
            optional(:deny) => integer
          },
          AuditLogEntry.reason()
        ) :: error | {:ok}
  def edit_channel_permissions(channel_id, overwrite_id, permission_info, reason \\ nil) do
    Nostrum.Api.Channel.edit_permissions(channel_id, overwrite_id, permission_info, reason)
  end

  @doc """
  Same as `edit_channel_permissions/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
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

  @doc """
  Delete a channel permission for a user or role.

  Role or user overwrite to delete is specified by `channel_id` and `overwrite_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec delete_channel_permissions(integer, integer, AuditLogEntry.reason()) :: error | {:ok}
  def delete_channel_permissions(channel_id, overwrite_id, reason \\ nil) do
    Nostrum.Api.Channel.delete_permissions(channel_id, overwrite_id, reason)
  end

  @doc ~S"""
  Gets a list of invites for a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'MANAGE_CHANNELS' permissions.

  If successful, returns `{:ok, invite}`. Otherwise, returns a
  `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_channel_invites(43189401384091)
  {:ok, [%Nostrum.Struct.Invite{} | _]}
  ```
  """
  @spec get_channel_invites(Channel.id()) :: error | {:ok, [Invite.detailed_invite()]}
  def get_channel_invites(channel_id) when is_snowflake(channel_id) do
    Nostrum.Api.Invite.channel_invites(channel_id)
  end

  @doc ~S"""
  Same as `get_channel_invites/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_channel_invites!(Channel.id()) :: no_return | [Invite.detailed_invite()]
  def get_channel_invites!(channel_id) do
    get_channel_invites(channel_id)
    |> bangify
  end

  @doc ~S"""
  Creates an invite for a guild channel.

  An optional `reason` can be provided for the audit log.

  This endpoint requires the `CREATE_INSTANT_INVITE` permission.

  If successful, returns `{:ok, invite}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:max_age` (integer) - duration of invite in seconds before expiry, or 0 for never.
      (default: `86400`)
    * `:max_uses` (integer) - max number of uses or 0 for unlimited.
      (default: `0`)
    * `:temporary` (boolean) - Whether the invite should grant temporary
      membership. (default: `false`)
    * `:unique` (boolean) - used when creating unique one time use invites.
      (default: `false`)

  ## Examples

  ```elixir
  Nostrum.Api.create_channel_invite(41771983423143933)
  {:ok, Nostrum.Struct.Invite{}}

  Nostrum.Api.create_channel_invite(41771983423143933, max_uses: 20)
  {:ok, %Nostrum.Struct.Invite{}}
  ```
  """
  @spec create_channel_invite(Channel.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Invite.detailed_invite()}
  def create_channel_invite(channel_id, options \\ [], reason \\ nil) do
    Nostrum.Api.Invite.create(channel_id, options, reason)
  end

  @doc ~S"""
  Same as `create_channel_invite/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_channel_invite!(Channel.id(), options, AuditLogEntry.reason()) ::
          no_return | Invite.detailed_invite()
  def create_channel_invite!(channel_id, options \\ [], reason \\ nil) do
    create_channel_invite(channel_id, options, reason)
    |> bangify
  end

  @doc """
  Triggers the typing indicator.

  Triggers the typing indicator in the channel specified by `channel_id`.
  The typing indicator lasts for about 8 seconds and then automatically stops.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec start_typing(integer) :: error | {:ok}
  def start_typing(channel_id) do
    Nostrum.Api.Channel.start_typing(channel_id)
  end

  @doc """
  Same as `start_typing/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec start_typing!(integer) :: no_return | {:ok}
  def start_typing!(channel_id) do
    start_typing(channel_id)
    |> bangify
  end

  @doc ~S"""
  Retrieves all pinned messages from a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'READ_MESSAGE_HISTORY' permissions.

  If successful, returns `{:ok, messages}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_pinned_messages(43189401384091)
  ```
  """
  @spec get_pinned_messages(Channel.id()) :: error | {:ok, [Message.t()]}
  def get_pinned_messages(channel_id) when is_snowflake(channel_id) do
    Nostrum.Api.Channel.pinned_messages(channel_id)
  end

  @doc ~S"""
  Same as `get_pinned_messages/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_pinned_messages!(Channel.id()) :: no_return | [Message.t()]
  def get_pinned_messages!(channel_id) do
    get_pinned_messages(channel_id)
    |> bangify
  end

  @doc ~S"""
  Pins a message in a channel.

  This endpoint requires the 'VIEW_CHANNEL', 'READ_MESSAGE_HISTORY', and
  'MANAGE_MESSAGES' permissions. It fires the
  `t:Nostrum.Consumer.message_update/0` and
  `t:Nostrum.Consumer.channel_pins_update/0` events.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.add_pinned_channel_message(43189401384091, 18743893102394)
  ```
  """
  @spec add_pinned_channel_message(Channel.id(), Message.id()) :: error | {:ok}
  def add_pinned_channel_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Nostrum.Api.Channel.pin_message(channel_id, message_id)
  end

  @doc ~S"""
  Same as `add_pinned_channel_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec add_pinned_channel_message!(Channel.id(), Message.id()) :: no_return | {:ok}
  def add_pinned_channel_message!(channel_id, message_id) do
    add_pinned_channel_message(channel_id, message_id)
    |> bangify
  end

  @doc """
  Unpins a message in a channel.

  This endpoint requires the 'VIEW_CHANNEL', 'READ_MESSAGE_HISTORY', and
  'MANAGE_MESSAGES' permissions. It fires the
  `t:Nostrum.Consumer.message_update/0` and
  `t:Nostrum.Consumer.channel_pins_update/0` events.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec delete_pinned_channel_message(Channel.id(), Message.id()) :: error | {:ok}
  def delete_pinned_channel_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Nostrum.Api.Channel.unpin_message(channel_id, message_id)
  end

  @doc ~S"""
  Same as `delete_pinned_channel_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_pinned_channel_message!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_pinned_channel_message!(channel_id, message_id) do
    delete_pinned_channel_message(channel_id, message_id)
    |> bangify
  end

  @doc ~S"""
  Gets a list of emojis for a given guild.

  This endpoint requires the `MANAGE_EMOJIS` permission.

  If successful, returns `{:ok, emojis}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec list_guild_emojis(Guild.id()) :: error | {:ok, [Emoji.t()]}
  def list_guild_emojis(guild_id) do
    Nostrum.Api.Guild.emojis(guild_id)
  end

  @doc ~S"""
  Same as `list_guild_emojis/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec list_guild_emojis!(Guild.id()) :: no_return | [Emoji.t()]
  def list_guild_emojis!(guild_id) do
    list_guild_emojis(guild_id)
    |> bangify
  end

  @doc ~S"""
  Gets an emoji for the given guild and emoji ids.

  This endpoint requires the `MANAGE_EMOJIS` permission.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec get_guild_emoji(Guild.id(), Emoji.id()) :: error | {:ok, Emoji.t()}
  def get_guild_emoji(guild_id, emoji_id) do
    Nostrum.Api.Guild.emoji(guild_id, emoji_id)
  end

  @doc ~S"""
  Same as `get_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_emoji!(Guild.id(), Emoji.id()) :: no_return | Emoji.t()
  def get_guild_emoji!(guild_id, emoji_id) do
    get_guild_emoji(guild_id, emoji_id)
    |> bangify
  end

  @doc ~S"""
  Creates a new emoji for the given guild.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  An optional `reason` can be provided for the audit log.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the emoji
    * `:image` (base64 data URI) - the 128x128 emoji image. Maximum size of 256kb
    * `:roles` (list of `t:Nostrum.Snowflake.t/0`) - roles for which this emoji will be whitelisted
    (default: [])

  `:name` and `:image` are always required.

  ## Examples

  ```elixir
  image = "data:image/png;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8="

  Nostrum.Api.create_guild_emoji(43189401384091, name: "nostrum", image: image, roles: [])
  ```
  """
  @spec create_guild_emoji(Guild.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Emoji.t()}
  def create_guild_emoji(guild_id, options, reason \\ nil) do
    Nostrum.Api.Guild.create_emoji(guild_id, options, reason)
  end

  @doc ~S"""
  Same as `create_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_emoji!(Guild.id(), options, AuditLogEntry.reason()) :: no_return | Emoji.t()
  def create_guild_emoji!(guild_id, params, reason \\ nil) do
    create_guild_emoji(guild_id, params, reason)
    |> bangify
  end

  @doc ~S"""
  Modify the given emoji.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  An optional `reason` can be provided for the audit log.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the emoji
    * `:roles` (list of `t:Nostrum.Snowflake.t/0`) - roles to which this emoji will be whitelisted

  ## Examples

  ```elixir
  Nostrum.Api.modify_guild_emoji(43189401384091, 4314301984301, name: "elixir", roles: [])
  ```
  """
  @spec modify_guild_emoji(Guild.id(), Emoji.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Emoji.t()}
  def modify_guild_emoji(guild_id, emoji_id, options \\ %{}, reason \\ nil) do
    Nostrum.Api.Guild.modify_emoji(guild_id, emoji_id, options, reason)
  end

  @doc ~S"""
  Same as `modify_guild_emoji/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_emoji!(Guild.id(), Emoji.id(), options, AuditLogEntry.reason()) ::
          no_return | Emoji.t()
  def modify_guild_emoji!(guild_id, emoji_id, options, reason \\ nil) do
    modify_guild_emoji(guild_id, emoji_id, options, reason)
    |> bangify
  end

  @doc ~S"""
  Deletes the given emoji.

  An optional `reason` can be provided for the audit log.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec delete_guild_emoji(Guild.id(), Emoji.id(), AuditLogEntry.reason()) :: error | {:ok}
  def delete_guild_emoji(guild_id, emoji_id, reason \\ nil) do
    Nostrum.Api.Guild.delete_emoji(guild_id, emoji_id, reason)
  end

  @doc ~S"""
  Same as `delete_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_guild_emoji!(Guild.id(), Emoji.id(), AuditLogEntry.reason()) :: no_return | {:ok}
  def delete_guild_emoji!(guild_id, emoji_id, reason \\ nil) do
    delete_guild_emoji(guild_id, emoji_id, reason)
    |> bangify
  end

  @doc ~S"""
  Fetch a sticker with the provided ID.

  Returns a `t:Nostrum.Struct.Sticker.t/0`.
  """
  @doc since: "0.10.0"
  @spec get_sticker(Snowflake.t()) :: {:ok, Sticker.t()} | error
  def get_sticker(sticker_id) do
    request(:get, Constants.sticker(sticker_id))
    |> handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  List all stickers in the provided guild.

  Returns a list of `t:Nostrum.Struct.Sticker.t/0`.
  """
  @doc since: "0.10.0"
  @spec list_guild_stickers(Guild.id()) :: {:ok, [Sticker.t()]} | error
  def list_guild_stickers(guild_id) do
    request(:get, Constants.guild_stickers(guild_id))
    |> handle_request_with_decode({:list, {:struct, Sticker}})
  end

  @doc ~S"""
  Return the specified sticker from the specified guild.

  Returns a `t:Nostrum.Struct.Sticker.t/0`.
  """
  @doc since: "0.10.0"
  @spec get_guild_sticker(Guild.id(), Sticker.id()) :: Sticker.t() | error
  def get_guild_sticker(guild_id, sticker_id) do
    request(:get, Constants.guild_sticker(guild_id, sticker_id))
    |> handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  Create a sticker in a guild.

  Every guild has five free sticker slots by default, and each Boost level will
  grant access to more slots.

  Uploaded stickers are constrained to 5 seconds in length for animated stickers, and 320 x 320 pixels.

  Stickers in the [Lottie file format](https://airbnb.design/lottie/) can only
  be uploaded on guilds that have either the `VERIFIED` and/or the `PARTNERED`
  guild feature.

  ## Parameters

  - `name`: Name of the sticker (2-30 characters)
  - `description`: Description of the sticker (2-100 characters)
  - `tags`: Autocomplete/suggestion tags for the sticker (max 200 characters)
  - `file`: A path to a file to upload or a map of `name` (file name) and `body` (file data).
  - `reason` (optional): audit log reason to attach to this event

  ## Returns

  Returns a `t:Nostrum.Struct.Sticker.t/0` on success.
  """
  @doc since: "0.10.0"
  @spec create_guild_sticker(
          Guild.id(),
          Sticker.name(),
          Sticker.description(),
          Sticker.tags(),
          String.t() | %{body: iodata(), name: String.t()},
          String.t() | nil
        ) :: {:ok, Sticker.t()} | error
  def create_guild_sticker(guild_id, name, description, tags, file, reason \\ nil) do
    opts = %{
      name: name,
      description: description,
      tags: tags
    }

    boundary = generate_boundary()

    multipart = create_multipart([], Jason.encode_to_iodata!(opts), boundary)

    headers =
      maybe_add_reason(reason, [
        {"content-type", "multipart/form-data; boundary=#{boundary}"}
      ])

    file = create_file_part_for_multipart(file, nil, boundary, "file")

    %{
      method: :post,
      route: Constants.guild_stickers(guild_id),
      body:
        {:multipart,
         [
           ~s|--#{boundary}#{@crlf}|,
           file
           | multipart
         ]},
      params: [],
      headers: headers
    }
    |> request()
    |> handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  Modify a guild sticker with the specified ID.

  Pass in a map of properties to update, with any of the following keys:

  - `name`: Name of the sticker (2-30 characters)
  - `description`: Description of the sticker (2-100 characters)
  - `tags`: Autocomplete/suggestion tags for the sticker (max 200 characters)

  Returns an updated sticker on update completion.
  """
  @doc since: "0.10.0"
  @spec modify_guild_sticker(Guild.id(), Sticker.id(), %{
          name: Sticker.name() | nil,
          description: Sticker.description() | nil,
          tags: Sticker.tags() | nil
        }) :: {:ok, Sticker.t()} | error
  def modify_guild_sticker(guild_id, sticker_id, options) do
    request(:patch, Constants.guild_sticker(guild_id, sticker_id), options)
    |> handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  Delete a guild sticker with the specified ID.
  """
  @doc since: "0.10.0"
  @spec delete_guild_sticker(Guild.id(), Sticker.id()) :: {:ok} | error
  def delete_guild_sticker(guild_id, sticker_id) do
    request(:delete, Constants.guild_sticker(guild_id, sticker_id))
  end

  @doc ~S"""
  Get a list of available sticker packs.
  """
  @doc since: "0.10.0"
  @spec get_sticker_packs() :: {:ok, [Sticker.Pack.t()]} | error
  def get_sticker_packs do
    resp =
      request(:get, Constants.sticker_packs())
      |> handle_request_with_decode()

    case resp do
      {:ok, %{sticker_packs: packs}} -> {:ok, Util.cast(packs, {:list, {:struct, Sticker.Pack}})}
      _ -> resp
    end
  end

  @doc ~S"""
  Get the `t:Nostrum.Struct.Guild.AuditLog.t/0` for the given `guild_id`.

  ## Options

    * `:user_id` (`t:Nostrum.Struct.User.id/0`) - filter the log for a user ID
    * `:action_type` (`t:integer/0`) - filter the log by audit log type, see [Audit Log Events](https://discord.com/developers/docs/resources/audit-log#audit-log-entry-object-audit-log-events)
    * `:before` (`t:Nostrum.Struct.Snowflake.t/0`) - filter the log before a certain entry ID
    * `:limit` (`t:pos_integer/0`) - how many entries are returned (default 50, minimum 1, maximum 100)
  """
  @spec get_guild_audit_log(Guild.id(), options) :: {:ok, AuditLog.t()} | error
  def get_guild_audit_log(guild_id, options \\ []) do
    Nostrum.Api.Guild.audit_log(guild_id, options)
  end

  @doc ~S"""
  Gets a guild.

  If successful, returns `{:ok, guild}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_guild(81384788765712384)
  {:ok, %Nostrum.Struct.Guild{id: 81384788765712384}}
  ```
  """
  @spec get_guild(Guild.id()) :: error | {:ok, Guild.rest_guild()}
  def get_guild(guild_id) when is_snowflake(guild_id) do
    Nostrum.Api.Guild.get(guild_id)
  end

  @doc """
  Same as `get_guild/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild!(Guild.id()) :: no_return | Guild.rest_guild()
  def get_guild!(guild_id) do
    get_guild(guild_id)
    |> bangify
  end

  @doc """
  Modifies a guild's settings.

  This endpoint requires the `MANAGE_GUILD` permission. It fires the
  `t:Nostrum.Consumer.guild_update/0` event.

  An optional `reason` can be provided for the audit log.

  If successful, returns `{:ok, guild}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - guild name
    * `:region` (string) - guild voice region id
    * `:verification_level` (integer) - verification level
    * `:default_message_notifications` (integer) - default message
    notification level
    * `:explicit_content_filter` (integer) - explicit content filter level
    * `:afk_channel_id` (`t:Nostrum.Snowflake.t/0`) - id for afk channel
    * `:afk_timeout` (integer) - afk timeout in seconds
    * `:icon` (base64 data URI) - 128x128 jpeg image for the guild icon
    * `:owner_id` (`t:Nostrum.Snowflake.t/0`) - user id to transfer
    guild ownership to (must be owner)
    * `:splash` (base64 data URI) - 128x128 jpeg image for the guild splash
    (VIP only)
    * `:system_channel_id` (`t:Nostrum.Snowflake.t/0`) - the id of the
    channel to which system messages are sent
    * `:rules_channel_id` (`t:Nostrum.Snowflake.t/0`) - the id of the channel that
    is used for rules in public guilds
    * `:public_updates_channel_id` (`t:Nostrum.Snowflake.t/0`) - the id of the channel
    where admins and moderators receive notices from Discord in public guilds

  ## Examples

  ```elixir
  Nostrum.Api.modify_guild(451824027976073216, name: "Nose Drum")
  {:ok, %Nostrum.Struct.Guild{id: 451824027976073216, name: "Nose Drum", ...}}
  ```
  """
  @spec modify_guild(Guild.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Guild.rest_guild()}
  def modify_guild(guild_id, options \\ [], reason \\ nil) do
    Nostrum.Api.Guild.modify(guild_id, options, reason)
  end

  @doc """
  Same as `modify_guild/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild!(Guild.id(), options) :: no_return | Guild.rest_guild()
  def modify_guild!(guild_id, options \\ []) do
    modify_guild(guild_id, options)
    |> bangify
  end

  @doc ~S"""
  Deletes a guild.

  This endpoint requires that the current user is the owner of the guild.
  It fires the `t:Nostrum.Consumer.guild_delete/0` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.delete_guild(81384788765712384)
  {:ok}
  ```
  """
  @spec delete_guild(Guild.id()) :: error | {:ok}
  def delete_guild(guild_id) when is_snowflake(guild_id) do
    Nostrum.Api.Guild.delete(guild_id)
  end

  @doc ~S"""
  Same as `delete_guild/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_guild!(Guild.id()) :: no_return | {:ok}
  def delete_guild!(guild_id) do
    delete_guild(guild_id)
    |> bangify
  end

  @doc ~S"""
  Gets a list of guild channels.

  If successful, returns `{:ok, channels}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_guild_channels(81384788765712384)
  {:ok, [%Nostrum.Struct.Channel{guild_id: 81384788765712384} | _]}
  ```
  """
  @spec get_guild_channels(Guild.id()) :: error | {:ok, [Channel.guild_channel()]}
  def get_guild_channels(guild_id) when is_snowflake(guild_id) do
    Nostrum.Api.Guild.channels(guild_id)
  end

  @doc ~S"""
  Same as `get_guild_channels/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_channels!(Guild.id()) :: no_return | [Channel.guild_channel()]
  def get_guild_channels!(guild_id) do
    get_guild_channels(guild_id)
    |> bangify
  end

  @doc """
  Creates a channel for a guild.

  This endpoint requires the `MANAGE_CHANNELS` permission. It fires a
  `t:Nostrum.Consumer.channel_create/0` event.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - channel name (2-100 characters)
    * `:type` (integer) - the type of channel (See `Nostrum.Struct.Channel`)
    * `:topic` (string) - channel topic (0-1024 characters)
    * `:bitrate` (integer) - the bitrate (in bits) of the voice channel (voice only)
    * `:user_limit` (integer) - the user limit of the voice channel (voice only)
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0` or equivalent map) -
    the channel's permission overwrites
    * `:parent_id` (`t:Nostrum.Struct.Channel.id/0`) - id of the parent category for a channel
    * `:nsfw` (boolean) - if the channel is nsfw

  `:name` is always required.

  ## Examples

  ```elixir
  Nostrum.Api.create_guild_channel(81384788765712384, name: "elixir-nostrum", topic: "craig's domain")
  {:ok, %Nostrum.Struct.Channel{guild_id: 81384788765712384}}
  ```
  """
  @spec create_guild_channel(Guild.id(), options) :: error | {:ok, Channel.guild_channel()}
  def create_guild_channel(guild_id, options) do
    Nostrum.Api.Channel.create(guild_id, options)
  end

  @doc ~S"""
  Same as `create_guild_channel/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_channel!(Guild.id(), options) :: no_return | Channel.guild_channel()
  def create_guild_channel!(guild_id, options) do
    create_guild_channel(guild_id, options)
    |> bangify
  end

  @doc """
  Reorders a guild's channels.

  This endpoint requires the `MANAGE_CHANNELS` permission. It fires multiple
  `t:Nostrum.Consumer.channel_update/0` events.

  If successful, returns `{:ok, channels}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  `positions` is a list of maps that each map a channel id with a position.

  ## Examples

  ```elixir
  Nostrum.Api.modify_guild_channel_positions(279093381723062272, [%{id: 351500354581692420, position: 2}])
  {:ok}
  ```
  """
  @spec modify_guild_channel_positions(Guild.id(), [%{id: integer, position: integer}]) ::
          error | {:ok}
  def modify_guild_channel_positions(guild_id, positions)
      when is_snowflake(guild_id) and is_list(positions) do
    Nostrum.Api.Guild.modify_channel_positions(guild_id, positions)
  end

  @doc ~S"""
  Same as `modify_guild_channel_positions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_channel_positions!(Guild.id(), [%{id: integer, position: integer}]) ::
          no_return | {:ok}
  def modify_guild_channel_positions!(guild_id, positions) do
    modify_guild_channel_positions(guild_id, positions)
    |> bangify
  end

  @doc """
  Gets a guild member.

  If successful, returns `{:ok, member}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_guild_member(4019283754613, 184937267485)
  ```
  """
  @spec get_guild_member(Guild.id(), User.id()) :: error | {:ok, Member.t()}
  def get_guild_member(guild_id, user_id) when is_snowflake(guild_id) and is_snowflake(user_id) do
    Nostrum.Api.Guild.member(guild_id, user_id)
  end

  @doc """
  Same as `get_guild_member/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_member!(Guild.id(), User.id()) :: no_return | Member.t()
  def get_guild_member!(guild_id, user_id) do
    get_guild_member(guild_id, user_id)
    |> bangify
  end

  @doc """
  Gets a list of a guild's members.

  If successful, returns `{:ok, members}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:limit` (integer) - max number of members to return (1-1000) (default: 1)
    * `:after` (`t:Nostrum.Struct.User.id/0`) - the highest user id in the previous page (default: 0)

  ## Examples

  ```elixir
  Nostrum.Api.list_guild_members(41771983423143937, limit: 1)
  ```
  """
  @spec list_guild_members(Guild.id(), options) :: error | {:ok, [Member.t()]}
  def list_guild_members(guild_id, options \\ %{}) do
    Nostrum.Api.Guild.members(guild_id, options)
  end

  @doc """
  Same as `list_guild_members/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec list_guild_members!(Guild.id(), options) :: no_return | [Member.t()]
  def list_guild_members!(guild_id, options \\ %{}) do
    list_guild_members(guild_id, options)
    |> bangify
  end

  @doc ~S"""
  Puts a user in a guild.

  This endpoint fires the `t:Nostrum.Consumer.guild_member_add/0` event.
  It requires the `CREATE_INSTANT_INVITE` permission. Additionally, it
  situationally requires the `MANAGE_NICKNAMES`, `MANAGE_ROLES`,
  `MUTE_MEMBERS`, and `DEAFEN_MEMBERS` permissions.

  If successful, returns `{:ok, member}` or `{:ok}` if the user was already a member of the
  guild. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:access_token` (string) - the user's oauth2 access token
    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Struct.Guild.Role.id/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened

  `:access_token` is always required.

  ## Examples

  ```elixir
  Nostrum.Api.add_guild_member(
    41771983423143937,
    18374719829378473,
    access_token: "6qrZcUqja7812RVdnEKjpzOL4CvHBFG",
    nick: "nostrum",
    roles: [431849301, 913809431]
  )
  ```
  """
  @spec add_guild_member(Guild.id(), User.id(), options) :: error | {:ok, Member.t()} | {:ok}
  def add_guild_member(guild_id, user_id, options) do
    Nostrum.Api.Guild.add_member(guild_id, user_id, options)
  end

  @doc """
  Same as `add_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec add_guild_member!(Guild.id(), User.id(), options) :: no_return | Member.t() | {:ok}
  def add_guild_member!(guild_id, user_id, options) do
    add_guild_member(guild_id, user_id, options)
    |> bangify
  end

  @doc ~S"""
  Modifies a guild member's attributes.

  This endpoint fires the `t:Nostrum.Consumer.guild_member_update/0` event.
  It situationally requires the `MANAGE_NICKNAMES`, `MANAGE_ROLES`,
  `MUTE_MEMBERS`, `DEAFEN_MEMBERS`, and `MOVE_MEMBERS` permissions.

  If successful, returns `{:ok, member}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options

    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Snowflake.t/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened
    * `:channel_id` (`t:Nostrum.Snowflake.t/0`) - id of channel to move user to (if they are connected to voice)
    * `:communication_disabled_until` (`t:DateTime.t/0` or `nil`) - datetime to disable user communication (timeout) until, or `nil` to remove timeout.

  ## Examples

  ```elixir
  Nostrum.Api.modify_guild_member(41771983423143937, 637162356451, nick: "Nostrum")
  {:ok, %Nostrum.Struct.Member{}}
  ```
  """
  @spec modify_guild_member(Guild.id(), User.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Member.t()}
  def modify_guild_member(guild_id, user_id, options \\ %{}, reason \\ nil) do
    Nostrum.Api.Guild.modify_member(guild_id, user_id, options, reason)
  end

  @doc """
  Same as `modify_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_member!(Guild.id(), User.id(), options, AuditLogEntry.reason()) ::
          error | {:ok}
  def modify_guild_member!(guild_id, user_id, options \\ %{}, reason \\ nil) do
    modify_guild_member(guild_id, user_id, options, reason)
    |> bangify
  end

  @doc """
  Modifies the nickname of the current user in a guild.

  If successful, returns `{:ok, %{nick: nick}}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:nick` (string) - value to set users nickname to

  ## Examples

  ```elixir
  Nostrum.Api.modify_current_user_nick(41771983423143937, nick: "Nostrum")
  {:ok, %{nick: "Nostrum"}}
  ```
  """
  @spec modify_current_user_nick(Guild.id(), options) :: error | {:ok, %{nick: String.t()}}
  def modify_current_user_nick(guild_id, options \\ %{}) do
    Nostrum.Api.Guild.modify_self_nick(guild_id, options)
  end

  @doc """
  Same as `modify_current_user_nick/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_current_user_nick!(Guild.id(), options) :: no_return | %{nick: String.t()}
  def modify_current_user_nick!(guild_id, options \\ %{}) do
    modify_current_user_nick(guild_id, options)
    |> bangify()
  end

  @doc """
  Adds a role to a member.

  Role to add is specified by `role_id`.
  User to add role to is specified by `guild_id` and `user_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec add_guild_member_role(integer, integer, integer, AuditLogEntry.reason()) :: error | {:ok}
  def add_guild_member_role(guild_id, user_id, role_id, reason \\ nil) do
    Nostrum.Api.Role.add_member(guild_id, user_id, role_id, reason)
  end

  @doc """
  Removes a role from a member.

  Role to remove is specified by `role_id`.
  User to remove role from is specified by `guild_id` and `user_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec remove_guild_member_role(integer, integer, integer, AuditLogEntry.reason()) ::
          error | {:ok}
  def remove_guild_member_role(guild_id, user_id, role_id, reason \\ nil) do
    Nostrum.Api.Role.remove_member(guild_id, user_id, role_id, reason)
  end

  @doc """
  Removes a member from a guild.

  This event requires the `KICK_MEMBERS` permission. It fires a
  `t:Nostrum.Consumer.guild_member_remove/0` event.

  An optional reason can be provided for the audit log with `reason`.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.remove_guild_member(1453827904102291, 18739485766253)
  {:ok}
  ```
  """
  @spec remove_guild_member(Guild.id(), User.id(), AuditLogEntry.reason()) :: error | {:ok}
  def remove_guild_member(guild_id, user_id, reason \\ nil)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    Nostrum.Api.Guild.kick_member(guild_id, user_id, reason)
  end

  @doc """
  Same as `remove_guild_member/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec remove_guild_member!(Guild.id(), User.id(), AuditLogEntry.reason()) :: no_return | {:ok}
  def remove_guild_member!(guild_id, user_id, reason \\ nil) do
    remove_guild_member(guild_id, user_id, reason)
    |> bangify
  end

  @doc """
  Gets a ban object for the given user from a guild.
  """
  @doc since: "0.5.0"
  @spec get_guild_ban(integer, integer) :: error | {:ok, Guild.Ban.t()}
  def get_guild_ban(guild_id, user_id) do
    Nostrum.Api.Guild.ban(guild_id, user_id)
  end

  @doc """
  Gets a list of users banned from a guild.

  Guild to get bans for is specified by `guild_id`.
  """
  @spec get_guild_bans(integer) :: error | {:ok, [Nostrum.Struct.User.t()]}
  def get_guild_bans(guild_id) do
    Nostrum.Api.Guild.bans(guild_id)
  end

  @doc """
  Bans a user from a guild.

  User to delete is specified by `guild_id` and `user_id`.
  An optional `reason` can be specified for the audit log.
  """
  @spec create_guild_ban(integer, integer, integer, AuditLogEntry.reason()) :: error | {:ok}
  def create_guild_ban(guild_id, user_id, days_to_delete, reason \\ nil) do
    Nostrum.Api.Guild.ban_member(guild_id, user_id, days_to_delete, reason)
  end

  @doc """
  Removes a ban for a user.

  User to unban is specified by `guild_id` and `user_id`.
  An optional `reason` can be specified for the audit log.
  """
  @spec remove_guild_ban(integer, integer, AuditLogEntry.reason()) :: error | {:ok}
  def remove_guild_ban(guild_id, user_id, reason \\ nil) do
    Nostrum.Api.Guild.unban_member(guild_id, user_id, reason)
  end

  @doc ~S"""
  Gets a guild's roles.

  If successful, returns `{:ok, roles}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_guild_roles(147362948571673)
  ```
  """
  @spec get_guild_roles(Guild.id()) :: error | {:ok, [Role.t()]}
  def get_guild_roles(guild_id) when is_snowflake(guild_id) do
    request(:get, Constants.guild_roles(guild_id))
    |> handle_request_with_decode({:list, {:struct, Role}})
  end

  @doc ~S"""
  Same as `get_guild_roles/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_roles!(Guild.id()) :: no_return | [Role.t()]
  def get_guild_roles!(guild_id) do
    get_guild_roles(guild_id)
    |> bangify
  end

  @doc ~S"""
  Creates a guild role.

  An optional reason for the audit log can be provided via `reason`.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_create/0` event.

  If successful, returns `{:ok, role}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the role (default: "new role")
    * `:permissions` (integer) - bitwise of the enabled/disabled permissions (default: @everyone perms)
    * `:color` (integer) - RGB color value (default: 0)
    * `:hoist` (boolean) - whether the role should be displayed separately in the sidebar (default: false)
    * `:mentionable` (boolean) - whether the role should be mentionable (default: false)
    * `:icon` (string) - URL role icon (default: `nil`)
    * `:unicode_emoji` (string) - standard unicode character emoji role icon (default: `nil`)

  ## Examples

  ```elixir
  Nostrum.Api.create_guild_role(41771983423143937, name: "nostrum-club", hoist: true)
  ```
  """
  @spec create_guild_role(Guild.id(), options, AuditLogEntry.reason()) :: error | {:ok, Role.t()}
  def create_guild_role(guild_id, options, reason \\ nil) do
    Nostrum.Api.Role.create(guild_id, options, reason)
  end

  @doc ~S"""
  Same as `create_guild_role/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_role!(Guild.id(), options, AuditLogEntry.reason()) :: no_return | Role.t()
  def create_guild_role!(guild_id, options, reason \\ nil) do
    create_guild_role(guild_id, options, reason)
    |> bangify
  end

  @doc ~S"""
  Reorders a guild's roles.

  This endpoint requires the `MANAGE_ROLES` permission. It fires multiple
  `t:Nostrum.Consumer.guild_role_update/0` events.

  If successful, returns `{:ok, roles}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  `positions` is a list of maps that each map a role id with a position.

  ## Examples

  ```elixir
  Nostrum.Api.modify_guild_role_positions(41771983423143937, [%{id: 41771983423143936, position: 2}])
  ```
  """
  @spec modify_guild_role_positions(
          Guild.id(),
          [%{id: Role.id(), position: integer}],
          AuditLogEntry.reason()
        ) :: error | {:ok, [Role.t()]}
  def modify_guild_role_positions(guild_id, positions, reason \\ nil)
      when is_snowflake(guild_id) and is_list(positions) do
    Nostrum.Api.Guild.modify_role_positions(guild_id, positions, reason)
  end

  @doc ~S"""
  Same as `modify_guild_role_positions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_role_positions!(
          Guild.id(),
          [%{id: Role.id(), position: integer}],
          AuditLogEntry.reason()
        ) :: no_return | [Role.t()]
  def modify_guild_role_positions!(guild_id, positions, reason \\ nil) do
    modify_guild_role_positions(guild_id, positions, reason)
    |> bangify
  end

  @doc ~S"""
  Modifies a guild role.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_update/0` event.

  An optional `reason` can be specified for the audit log.

  If successful, returns `{:ok, role}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the role
    * `:permissions` (integer) - bitwise of the enabled/disabled permissions
    * `:color` (integer) - RGB color value (default: 0)
    * `:hoist` (boolean) - whether the role should be displayed separately in the sidebar
    * `:mentionable` (boolean) - whether the role should be mentionable

  ## Examples

  ```elixir
  Nostrum.Api.modify_guild_role(41771983423143937, 392817238471936, hoist: false, name: "foo-bar")
  ```
  """
  @spec modify_guild_role(Guild.id(), Role.id(), options, AuditLogEntry.reason()) ::
          error | {:ok, Role.t()}
  def modify_guild_role(guild_id, role_id, options, reason \\ nil)

  def modify_guild_role(guild_id, role_id, options, reason) when is_list(options),
    do: modify_guild_role(guild_id, role_id, Map.new(options), reason)

  def modify_guild_role(guild_id, role_id, %{} = options, reason)
      when is_snowflake(guild_id) and is_snowflake(role_id) do
    %{
      method: :patch,
      route: Constants.guild_role(guild_id, role_id),
      body: options,
      params: [],
      headers: maybe_add_reason(reason)
    }
    |> request()
    |> handle_request_with_decode({:struct, Role})
  end

  @doc ~S"""
  Same as `modify_guild_role/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_role!(Guild.id(), Role.id(), options, AuditLogEntry.reason()) ::
          no_return | Role.t()
  def modify_guild_role!(guild_id, role_id, options, reason \\ nil) do
    modify_guild_role(guild_id, role_id, options, reason)
    |> bangify
  end

  @doc ~S"""
  Deletes a role from a guild.

  An optional `reason` can be specified for the audit log.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_delete/0` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.delete_guild_role(41771983423143937, 392817238471936)
  ```
  """
  @spec delete_guild_role(Guild.id(), Role.id(), AuditLogEntry.reason()) :: error | {:ok}
  def delete_guild_role(guild_id, role_id, reason \\ nil)
      when is_snowflake(guild_id) and is_snowflake(role_id) do
    Nostrum.Api.Role.delete(guild_id, role_id, reason)
  end

  @doc ~S"""
  Same as `delete_guild_role/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_guild_role!(Guild.id(), Role.id(), AuditLogEntry.reason()) :: no_return | {:ok}
  def delete_guild_role!(guild_id, role_id, reason \\ nil) do
    delete_guild_role(guild_id, role_id, reason)
    |> bangify
  end

  @doc """
  Gets the number of members that would be removed in a prune given `days`.

  This endpoint requires the `KICK_MEMBERS` permission.

  If successful, returns `{:ok, %{pruned: pruned}}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_guild_prune_count(81384788765712384, 1)
  {:ok, %{pruned: 0}}
  ```
  """
  @spec get_guild_prune_count(Guild.id(), 1..30) :: error | {:ok, %{pruned: integer}}
  def get_guild_prune_count(guild_id, days) when is_snowflake(guild_id) and days in 1..30 do
    request(:get, Constants.guild_prune(guild_id), "", days: days)
    |> handle_request_with_decode
  end

  @doc ~S"""
  Same as `get_guild_prune_count/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_prune_count!(Guild.id(), 1..30) :: no_return | %{pruned: integer}
  def get_guild_prune_count!(guild_id, days) do
    get_guild_prune_count(guild_id, days)
    |> bangify
  end

  @doc """
  Begins a guild prune to prune members within `days`.

  An optional `reason` can be provided for the guild audit log.

  This endpoint requires the `KICK_MEMBERS` permission. It fires multiple
  `t:Nostrum.Consumer.guild_member_remove/0` events.

  If successful, returns `{:ok, %{pruned: pruned}}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.begin_guild_prune(81384788765712384, 1)
  {:ok, %{pruned: 0}}
  ```
  """
  @spec begin_guild_prune(Guild.id(), 1..30, AuditLogEntry.reason()) ::
          error | {:ok, %{pruned: integer}}
  def begin_guild_prune(guild_id, days, reason \\ nil)
      when is_snowflake(guild_id) and days in 1..30 do
    Nostrum.Api.Guild.begin_prune(guild_id, days, reason)
  end

  @doc ~S"""
  Same as `begin_guild_prune/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec begin_guild_prune!(Guild.id(), 1..30, AuditLogEntry.reason()) ::
          no_return | %{pruned: integer}
  def begin_guild_prune!(guild_id, days, reason) do
    begin_guild_prune(guild_id, days, reason)
    |> bangify
  end

  @doc """
  Gets a list of voice regions for the guild.

  Guild to get voice regions for is specified by `guild_id`.
  """
  @spec get_voice_region(integer) :: error | {:ok, [Nostrum.Struct.VoiceRegion.t()]}
  def get_voice_region(guild_id) do
    request(:get, Constants.guild_voice_regions(guild_id))
    |> handle_request_with_decode
  end

  @doc ~S"""
  Gets a list of invites for a guild.

  This endpoint requires the `MANAGE_GUILD` permission.

  If successful, returns `{:ok, invites}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_guild_invites(81384788765712384)
  {:ok, [%Nostrum.Struct.Invite{} | _]}
  ```
  """
  @spec get_guild_invites(Guild.id()) :: error | {:ok, [Invite.detailed_invite()]}
  def get_guild_invites(guild_id) when is_snowflake(guild_id) do
    Nostrum.Api.Invite.guild_invites(guild_id)
  end

  @doc ~S"""
  Same as `get_guild_invites/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_invites!(Guild.id()) :: no_return | [Invite.detailed_invite()]
  def get_guild_invites!(guild_id) do
    get_guild_invites(guild_id)
    |> bangify
  end

  @doc """
  Gets a list of guild integerations.

  Guild to get integrations for is specified by `guild_id`.
  """
  @spec get_guild_integrations(Guild.id()) ::
          error | {:ok, [Nostrum.Struct.Guild.Integration.t()]}
  def get_guild_integrations(guild_id) do
    request(:get, Constants.guild_integrations(guild_id))
    |> handle_request_with_decode
  end

  @doc """
  Creates a new guild integeration.

  Guild to create integration with is specified by `guild_id`.

  `options` is a map with the following requires keys:
   * `type` - Integration type.
   * `id` - Integeration id.
  """
  @spec create_guild_integrations(integer, %{
          type: String.t(),
          id: integer
        }) :: error | {:ok}
  def create_guild_integrations(guild_id, options) do
    Nostrum.Api.Guild.create_integration(guild_id, options)
  end

  @doc """
  Changes the settings and behaviours for a guild integeration.

  Integration to modify is specified by `guild_id` and `integeration_id`.

  `options` is a map with the following keys:
   * `expire_behavior` - Expiry behavior.
   * `expire_grace_period` - Period where the integration will ignore elapsed subs.
   * `enable_emoticons` - Whether emoticons should be synced.
  """
  @spec modify_guild_integrations(integer, integer, %{
          expire_behaviour: integer,
          expire_grace_period: integer,
          enable_emoticons: boolean
        }) :: error | {:ok}
  def modify_guild_integrations(guild_id, integration_id, options) do
    Nostrum.Api.Guild.modify_integration(guild_id, integration_id, options)
  end

  @doc """
  Deletes a guild integeration.

  Integration to delete is specified by `guild_id` and `integeration_id`.
  """
  @spec delete_guild_integrations(integer, integer) :: error | {:ok}
  def delete_guild_integrations(guild_id, integration_id) do
    Nostrum.Api.Guild.delete_integration(guild_id, integration_id)
  end

  @doc """
  Syncs a guild integration.

  Integration to sync is specified by `guild_id` and `integeration_id`.
  """
  @spec sync_guild_integrations(integer, integer) :: error | {:ok}
  def sync_guild_integrations(guild_id, integration_id) do
    Nostrum.Api.Guild.sync_integration(guild_id, integration_id)
  end

  @doc """
  Gets a guild embed.
  """
  @spec get_guild_widget(integer) :: error | {:ok, map}
  def get_guild_widget(guild_id) do
    Nostrum.Api.Guild.widget(guild_id)
  end

  @doc """
  Modifies a guild embed.
  """
  @spec modify_guild_widget(integer, map) :: error | {:ok, map}
  def modify_guild_widget(guild_id, options) do
    Nostrum.Api.Guild.modify_widget(guild_id, options)
  end

  @doc """
  Creates a new scheduled event for the guild.

  ## Options
    * `:channel_id` - (`t:Nostrum.Snowflake.t/0`) optional channel id for the event
    * `:entity_metadata` - (`t:Nostrum.Struct.Guild.ScheduledEvent.EntityMetadata.t/0`) metadata for the event
    * `:name` - (string) required name for the event
    * `:privacy_level` - (integer) at the time of writing, this must always be 2 for `GUILD_ONLY`
    * `:scheduled_start_time` - required time for the event to start as a `DateTime` or (ISO8601 timestamp)[`DateTime.to_iso8601/3`]
    * `:scheduled_end_time` - optional time for the event to end as a `DateTime` or (ISO8601 timestamp)[`DateTime.to_iso8601/3`]
    * `:description` - (string) optional description for the event
    * `:entity_type` - (integer) an integer representing the type of entity the event is for
      * `1` - `STAGE_INSTANCE`
      * `2` - `VOICE`
      * `3` - `EXTERNAL`

  See the (official documentation)[https://discord.com/developers/docs/resources/guild-scheduled-event] for more information.


  An optional `reason` can be specified for the audit log.
  """
  @doc since: "0.5.0"
  @spec create_guild_scheduled_event(Guild.id(), reason :: AuditLogEntry.reason(), options) ::
          {:ok, ScheduledEvent.t()} | error
  def create_guild_scheduled_event(guild_id, reason \\ nil, options) do
    Nostrum.Api.ScheduledEvent.create(guild_id, reason, options)
  end

  @doc """
  Get a list of scheduled events for a guild.
  """
  @doc since: "0.5.0"
  @spec get_guild_scheduled_events(Guild.id()) :: error | {:ok, [ScheduledEvent.t()]}
  def get_guild_scheduled_events(guild_id) do
    Nostrum.Api.Guild.scheduled_events(guild_id)
  end

  @doc """
  Get a scheduled event for a guild.
  """
  @doc since: "0.5.0"
  @spec get_guild_scheduled_event(Guild.id(), ScheduledEvent.id()) ::
          error | {:ok, ScheduledEvent.t()}
  def get_guild_scheduled_event(guild_id, event_id) do
    Nostrum.Api.ScheduledEvent.get(guild_id, event_id)
  end

  @doc """
  Delete a scheduled event for a guild.
  """
  @doc since: "0.5.0"
  @spec delete_guild_scheduled_event(Guild.id(), ScheduledEvent.id()) ::
          error | {:ok}
  def delete_guild_scheduled_event(guild_id, event_id) do
    Nostrum.Api.ScheduledEvent.delete(guild_id, event_id)
  end

  @doc """
  Modify a scheduled event for a guild.

  Options are the same as for `create_guild_scheduled_event/2` except all fields are optional,
  with the additional optional integer field `:status` which can be one of:

    * `1` - `SCHEDULED`
    * `2` - `ACTIVE`
    * `3` - `COMPLETED`
    * `4` - `CANCELLED`

  Copied from the official documentation:
  * If updating entity_type to `EXTERNAL`:
    * `channel_id` is required and must be set to null
    * `entity_metadata` with a `location` field must be provided
    * `scheduled_end_time` must be provided
  """
  @doc since: "0.5.0"
  @spec modify_guild_scheduled_event(
          Guild.id(),
          ScheduledEvent.id(),
          reason :: AuditLogEntry.reason(),
          options
        ) :: error | {:ok, ScheduledEvent.t()}
  def modify_guild_scheduled_event(guild_id, event_id, reason \\ nil, options) do
    Nostrum.Api.ScheduledEvent.modify(guild_id, event_id, reason, options)
  end

  @doc """
  Get a list of users who have subscribed to an event.

  ## Options
  All are optional, with their default values listed.
  * `:limit` (integer) maximum number of users to return, defaults to `100`
  * `:with_member` (boolean) whether to include the member object for each user, defaults to `false`
  * `:before` (`t:Nostrum.Snowflake.t/0`) return only users before this user id, defaults to `nil`
  * `:after` (`t:Nostrum.Snowflake.t/0`) return only users after this user id, defaults to `nil`
  """
  @doc since: "0.5.0"
  @spec get_guild_scheduled_event_users(Guild.id(), ScheduledEvent.id(), options) ::
          error | {:ok, [ScheduledEvent.User.t()]}
  def get_guild_scheduled_event_users(guild_id, event_id, params \\ []) do
    Nostrum.Api.ScheduledEvent.users(guild_id, event_id, params)
  end

  @doc ~S"""
  Gets an invite by its `invite_code`.

  If successful, returns `{:ok, invite}`. Otherwise, returns a
  `t:Nostrum.Api.error/0`.

  ## Options

    * `:with_counts` (boolean) - whether to include member count fields

  ## Examples

  ```elixir
  Nostrum.Api.get_invite("zsjUsC")

  Nostrum.Api.get_invite("zsjUsC", with_counts: true)
  ```
  """
  @spec get_invite(Invite.code(), options) :: error | {:ok, Invite.simple_invite()}
  def get_invite(invite_code, options \\ []) when is_binary(invite_code) do
    Nostrum.Api.Invite.get(invite_code, options)
  end

  @doc ~S"""
  Same as `get_invite/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_invite!(Invite.code(), options) :: no_return | Invite.simple_invite()
  def get_invite!(invite_code, options \\ []) do
    get_invite(invite_code, options)
    |> bangify
  end

  @doc ~S"""
  Deletes an invite by its `invite_code`.

  This endpoint requires the `MANAGE_CHANNELS` permission.

  If successful, returns `{:ok, invite}`. Otherwise, returns a
  `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.delete_invite("zsjUsC")
  ```
  """
  @spec delete_invite(Invite.code()) :: error | {:ok, Invite.simple_invite()}
  def delete_invite(invite_code) when is_binary(invite_code) do
    Nostrum.Api.Invite.delete(invite_code)
  end

  @doc ~S"""
  Same as `delete_invite/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_invite!(Invite.code()) :: no_return | Invite.simple_invite()
  def delete_invite!(invite_code) do
    delete_invite(invite_code)
    |> bangify
  end

  @doc """
  Gets a user by its `t:Nostrum.Struct.User.id/0`.

  If the request is successful, this function returns `{:ok, user}`, where
  `user` is a `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get_user(User.id()) :: error | {:ok, User.t()}
  def get_user(user_id) do
    Nostrum.Api.User.get(user_id)
  end

  @doc """
  Same as `get_user/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_user!(User.id()) :: no_return | User.t()
  def get_user!(user_id) do
    get_user(user_id)
    |> bangify
  end

  @doc """
  Gets info on the current user.

  If nostrum's caching is enabled, it is recommended to use `Me.get/0`
  instead of this function. This is because sending out an API request is much slower
  than pulling from our cache.

  If the request is successful, this function returns `{:ok, user}`, where
  `user` is nostrum's `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get_current_user() :: error | {:ok, User.t()}
  def get_current_user do
    Nostrum.Api.Self.get()
  end

  @doc """
  Same as `get_current_user/0`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_current_user!() :: no_return | User.t()
  def get_current_user! do
    get_current_user()
    |> bangify
  end

  @doc ~S"""
  Changes the username or avatar of the current user.

  ## Options

    * `:username` (string) - new username
    * `:avatar` (string) - the user's avatar as [avatar data](https://discord.com/developers/docs/resources/user#avatar-data)

  ## Examples

  ```elixir
  Nostrum.Api.modify_current_user(avatar: "data:image/jpeg;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8=")
  ```
  """
  @spec modify_current_user(options) :: error | {:ok, User.t()}
  def modify_current_user(options) do
    Nostrum.Api.Self.modify(options)
  end

  @doc """
  Same as `modify_current_user/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_current_user!(options) :: no_return | User.t()
  def modify_current_user!(options) do
    modify_current_user(options)
    |> bangify
  end

  @doc """
  Gets a list of guilds the user is currently in.

  This endpoint requires the `guilds` OAuth2 scope.

  If successful, returns `{:ok, guilds}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:before` (`t:Nostrum.Snowflake.t/0`) - get guilds before this
    guild ID
    * `:after` (`t:Nostrum.Snowflake.t/0`) - get guilds after this guild
    ID
    * `:limit` (integer) - max number of guilds to return (1-100)

  ## Examples

  ```elixir
  iex> Nostrum.Api.get_current_user_guilds(limit: 1)
  {:ok, [%Nostrum.Struct.Guild{}]}
  ```
  """
  @spec get_current_user_guilds(options) :: error | {:ok, [Guild.user_guild()]}
  def get_current_user_guilds(options \\ []) do
    Nostrum.Api.Self.guilds(options)
  end

  @doc ~S"""
  Same as `get_current_user_guilds/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_current_user_guilds!(options) :: no_return | [Guild.user_guild()]
  def get_current_user_guilds!(options \\ []) do
    get_current_user_guilds(options)
    |> bangify
  end

  @doc """
  Leaves a guild.

  Guild to leave is specified by `guild_id`.
  """
  @spec leave_guild(integer) :: error | {:ok}
  def leave_guild(guild_id) do
    Nostrum.Api.Guild.leave(guild_id)
  end

  @doc """
  Gets a list of our user's DM channels.

  If successful, returns `{:ok, dm_channels}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.get_user_dms()
  {:ok, [%Nostrum.Struct.Channel{type: 1} | _]}
  ```
  """
  @spec get_user_dms() :: error | {:ok, [Channel.dm_channel()]}
  def get_user_dms do
    Nostrum.Api.Self.dms()
  end

  @doc ~S"""
  Same as `get_user_dms/0`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_user_dms!() :: no_return | [Channel.dm_channel()]
  def get_user_dms! do
    get_user_dms()
    |> bangify
  end

  @doc ~S"""
  Create a new DM channel with a user.

  If successful, returns `{:ok, dm_channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.create_dm(150061853001777154)
  {:ok, %Nostrum.Struct.Channel{type: 1}}
  ```
  """
  @spec create_dm(User.id()) :: error | {:ok, Channel.dm_channel()}
  def create_dm(user_id) when is_snowflake(user_id) do
    Nostrum.Api.User.create_dm(user_id)
  end

  @doc ~S"""
  Same as `create_dm/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_dm!(User.id()) :: no_return | Channel.dm_channel()
  def create_dm!(user_id) do
    create_dm(user_id)
    |> bangify
  end

  @doc """
  Creates a new group DM channel.

  If successful, returns `{:ok, group_dm_channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  `access_tokens` are user oauth2 tokens. `nicks` is a map that maps a user id
  to a nickname.

  ## Examples

  ```elixir
  Nostrum.Api.create_group_dm(["6qrZcUqja7812RVdnEKjpzOL4CvHBFG"], %{41771983423143937 => "My Nickname"})
  {:ok, %Nostrum.Struct.Channel{type: 3}}
  ```
  """
  @spec create_group_dm([String.t()], %{optional(User.id()) => String.t()}) ::
          error | {:ok, Channel.group_dm_channel()}
  def create_group_dm(access_tokens, nicks) when is_list(access_tokens) and is_map(nicks) do
    Nostrum.Api.User.create_group_dm(access_tokens, nicks)
  end

  @doc ~S"""
  Same as `create_group_dm/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_group_dm!([String.t()], %{optional(User.id()) => String.t()}) ::
          no_return | Channel.group_dm_channel()
  def create_group_dm!(access_tokens, nicks) do
    create_group_dm(access_tokens, nicks)
    |> bangify
  end

  @doc """
  Gets a list of user connections.
  """
  @spec get_user_connections() :: error | {:ok, list()}
  def get_user_connections do
    Nostrum.Api.Self.connections()
  end

  @doc """
  Gets a list of voice regions.
  """
  @spec list_voice_regions() :: error | {:ok, [Nostrum.Struct.VoiceRegion.t()]}
  def list_voice_regions do
    Nostrum.Api.Guild.voice_regions()
  end

  @doc """
  Creates a webhook.

  ## Parameters
    - `channel_id` - Id of the channel to send the message to.
    - `args` - Map with the following **required** keys:
      - `name` - Name of the webhook.
      - `avatar` - Base64 128x128 jpeg image for the default avatar.
    - `reason` - An optional reason for the guild audit log.
  """
  @spec create_webhook(
          Channel.id(),
          %{
            name: String.t(),
            avatar: String.t()
          },
          AuditLogEntry.reason()
        ) :: error | {:ok, Nostrum.Struct.Webhook.t()}
  def create_webhook(channel_id, args, reason \\ nil) do
    Nostrum.Api.Webhook.create(channel_id, args, reason)
  end

  @doc """
  Retrieves the original message of a webhook.
  """
  @doc since: "0.7.0"
  @spec get_webhook_message(Webhook.t(), Message.id()) ::
          error | {:ok, Message.t()}
  def get_webhook_message(webhook, message_id) do
    Nostrum.Api.Webhook.get_message(webhook, message_id)
  end

  @doc """
  Gets a list of webhooks for a channel.

  ## Parameters
    - `channel_id` - Channel to get webhooks for.
  """
  @spec get_channel_webhooks(Channel.id()) :: error | {:ok, [Nostrum.Struct.Webhook.t()]}
  def get_channel_webhooks(channel_id) do
    Nostrum.Api.Channel.webhooks(channel_id)
  end

  @doc """
  Gets a list of webhooks for a guild.

  ## Parameters
    - `guild_id` - Guild to get webhooks for.
  """
  @spec get_guild_webhooks(Guild.id()) :: error | {:ok, [Nostrum.Struct.Webhook.t()]}
  def get_guild_webhooks(guild_id) do
    Nostrum.Api.Guild.webhooks(guild_id)
  end

  @doc """
  Gets a webhook by id.

  ## Parameters
    - `webhook_id` - Id of the webhook to get.
  """
  @spec get_webhook(Webhook.id()) :: error | {:ok, Nostrum.Struct.Webhook.t()}
  def get_webhook(webhook_id) do
    Nostrum.Api.Webhook.get(webhook_id)
  end

  @doc """
  Gets a webhook by id and token.

  This method is exactly like `get_webhook/1` but does not require
  authentication.

  ## Parameters
    - `webhook_id` - Id of the webhook to get.
    - `webhook_token` - Token of the webhook to get.
  """
  @spec get_webhook_with_token(Webhook.id(), Webhook.token()) ::
          error | {:ok, Nostrum.Struct.Webhook.t()}
  def get_webhook_with_token(webhook_id, webhook_token) do
    Nostrum.Api.Webhook.get_with_token(webhook_id, webhook_token)
  end

  @doc """
  Modifies a webhook.

  ## Parameters
    - `webhook_id` - Id of the webhook to modify.
    - `args` - Map with the following *optional* keys:
      - `name` - Name of the webhook.
      - `avatar` - Base64 128x128 jpeg image for the default avatar.
    - `reason` - An optional reason for the guild audit log.
  """
  @spec modify_webhook(
          Webhook.id(),
          %{
            name: String.t(),
            avatar: String.t()
          },
          AuditLogEntry.reason()
        ) :: error | {:ok, Nostrum.Struct.Webhook.t()}
  def modify_webhook(webhook_id, args, reason \\ nil) do
    Nostrum.Api.Webhook.modify(webhook_id, args, reason)
  end

  @doc """
  Modifies a webhook with a token.

  This method is exactly like `modify_webhook/1` but does not require
  authentication.

  ## Parameters
    - `webhook_id` - Id of the webhook to modify.
    - `webhook_token` - Token of the webhook to get.
    - `args` - Map with the following *optional* keys:
      - `name` - Name of the webhook.
      - `avatar` - Base64 128x128 jpeg image for the default avatar.
    - `reason` - An optional reason for the guild audit log.
  """
  @spec modify_webhook_with_token(
          Webhook.id(),
          Webhook.token(),
          %{
            name: String.t(),
            avatar: String.t()
          },
          AuditLogEntry.reason()
        ) :: error | {:ok, Nostrum.Struct.Webhook.t()}
  def modify_webhook_with_token(webhook_id, webhook_token, args, reason \\ nil) do
    Nostrum.Api.Webhook.modify_with_token(webhook_id, webhook_token, args, reason)
  end

  @doc """
  Deletes a webhook.

  ## Parameters
    - `webhook_id` - Id of webhook to delete.
    - `reason` - An optional reason for the guild audit log.
  """
  @spec delete_webhook(Webhook.id(), AuditLogEntry.reason()) :: error | {:ok}
  def delete_webhook(webhook_id, reason \\ nil) do
    Nostrum.Api.Webhook.delete(webhook_id, reason)
  end

  @typep m1 :: %{
           required(:content) => String.t(),
           optional(:username) => String.t(),
           optional(:avatar_url) => String.t(),
           optional(:tts) => boolean,
           optional(:files) => [String.t() | %{body: iodata(), name: String.t()}],
           optional(:flags) => non_neg_integer(),
           optional(:thread_id) => Snowflake.t(),
           optional(:embeds) => nonempty_list(Embed.t()) | nil,
           optional(:allowed_mentions) => allowed_mentions()
         }

  @typep m2 ::
           %{
             optional(:content) => String.t() | nil,
             optional(:username) => String.t(),
             optional(:avatar_url) => String.t(),
             optional(:tts) => boolean,
             required(:files) => [String.t() | %{body: iodata(), name: String.t()}],
             optional(:flags) => non_neg_integer(),
             optional(:thread_id) => Snowflake.t(),
             optional(:embeds) => nonempty_list(Embed.t()) | nil,
             optional(:allowed_mentions) => allowed_mentions()
           }

  @typep m3 ::
           %{
             optional(:content) => String.t() | nil,
             optional(:username) => String.t(),
             optional(:avatar_url) => String.t(),
             optional(:tts) => boolean,
             optional(:files) => [String.t() | %{body: iodata(), name: String.t()}],
             optional(:flags) => non_neg_integer(),
             optional(:thread_id) => Snowflake.t(),
             required(:embeds) => nonempty_list(Embed.t()),
             optional(:allowed_mentions) => allowed_mentions()
           }

  @type matrix :: m1 | m2 | m3

  @spec execute_webhook(
          Webhook.id() | User.id(),
          Webhook.token() | Interaction.token(),
          matrix,
          boolean
        ) ::
          error | {:ok} | {:ok, Message.t()}

  @doc """
  Executes a webhook.

  ## Parameters
  - `webhook_id` - Id of the webhook to execute.
  - `webhook_token` - Token of the webhook to execute.
  - `args` - Map with the following allowed keys:
    - `content` - Message content.
    - `files` - List of Files to send.
    - `embeds` - List of embeds to send.
    - `username` - Overrides the default name of the webhook.
    - `avatar_url` - Overrides the default avatar of the webhook.
    - `tts` - Whether the message should be read over text to speech.
    - `flags` - Bitwise flags.
    - `thread_id` - Send a message to the specified thread within the webhook's channel.
    - `allowed_mentions` - Mentions to allow in the webhook message
  - `wait` - Whether to return an error or not. Defaults to `false`.

  **Note**: If `wait` is `true`, this method will return a `Message.t()` on success.

  At least one of `content`, `files` or `embeds` should be supplied in the `args` parameter.
  """

  def execute_webhook(webhook_id, webhook_token, args, wait \\ false) do
    Nostrum.Api.Webhook.execute(webhook_id, webhook_token, args, wait)
  end

  @doc """
  Edits a message previously created by the same webhook,
  args are the same as `execute_webhook/3`,
  however all fields are optional.
  """
  @doc since: "0.5.0"
  @spec edit_webhook_message(
          Webhook.id(),
          Webhook.token(),
          Message.id(),
          map()
        ) ::
          error | {:ok, Message.t()}
  def edit_webhook_message(webhook_id, webhook_token, message_id, args) do
    Nostrum.Api.Webhook.edit_message(webhook_id, webhook_token, message_id, args)
  end

  @doc """
  Executes a slack webhook.

  ## Parameters
    - `webhook_id` - Id of the webhook to execute.
    - `webhook_token` - Token of the webhook to execute.
  """
  @spec execute_slack_webhook(Webhook.id(), Webhook.token(), boolean) :: error | {:ok}
  def execute_slack_webhook(webhook_id, webhook_token, wait \\ false) do
    Nostrum.Api.Webhook.execute_slack(webhook_id, webhook_token, wait)
  end

  @doc """
  Executes a git webhook.

  ## Parameters
    - `webhook_id` - Id of the webhook to execute.
    - `webhook_token` - Token of the webhook to execute.
  """
  @spec execute_git_webhook(Webhook.id(), Webhook.token(), boolean) :: error | {:ok}
  def execute_git_webhook(webhook_id, webhook_token, wait \\ false) do
    Nostrum.Api.Webhook.execute_git(webhook_id, webhook_token, wait)
  end

  @doc """
  Gets the bot's OAuth2 application info.

  ## Example
  ```elixir
  Nostrum.Api.get_application_information
  {:ok,
  %{
    bot_public: false,
    bot_require_code_grant: false,
    description: "Test",
    icon: nil,
    id: "172150183260323840",
    name: "Baba O-Riley",
    owner: %{
      avatar: nil,
      discriminator: "0042",
      id: "172150183260323840",
      username: "i own a bot"
    },
  }}
  ```
  """
  @spec get_application_information() :: error | {:ok, map()}
  def get_application_information do
    Nostrum.Api.Self.application_information()
  end

  @doc """
  Fetch all global commands.

  ## Parameters
  - `application_id`: Application ID for which to search commands.
    If not given, this will be fetched from `Me`.

  ## Return value
  A list of ``ApplicationCommand``s on success. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure

  ## Example

  ```elixir
  iex> Nostrum.Api.get_global_application_commands
  {:ok,
   [
     %{
       application_id: "455589479713865749",
       description: "ed, man! man, ed",
       id: "789841753196331029",
       name: "edit"
     }
   ]}
  ```
  """
  @spec get_global_application_commands() :: {:ok, [map()]} | error
  @spec get_global_application_commands(User.id()) :: {:ok, [map()]} | error
  def get_global_application_commands(application_id \\ Me.get().id) do
    Nostrum.Api.ApplicationCommand.global_commands(application_id)
  end

  @doc """
  Create a new global application command.

  The new command will be available on all guilds in around an hour.
  If you want to test commands, use `create_guild_application_command/2` instead,
  as commands will become available instantly there.
  If an existing command with the same name exists, it will be overwritten.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The created command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#create-global-application-command

  ## Example

  ```elixir
  Nostrum.Api.create_global_application_command(
    %{name: "edit", description: "ed, man! man, ed", options: []}
  )
  ```
  """
  @spec create_global_application_command(ApplicationCommand.application_command_map()) ::
          {:ok, map()} | error
  @spec create_global_application_command(User.id(), ApplicationCommand.application_command_map()) ::
          {:ok, map()} | error
  def create_global_application_command(application_id \\ Me.get().id, command) do
    Nostrum.Api.ApplicationCommand.create_global_command(application_id, command)
  end

  @doc """
  Update an existing global application command.

  The updated command will be available on all guilds in around an hour.

  ## Parameters
  - `application_id`: Application ID for which to edit the command.
    If not given, this will be fetched from `Me`.
  - `command_id`: The current snowflake of the command.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The updated command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
  """
  @spec edit_global_application_command(
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) :: {:ok, map()} | error
  @spec edit_global_application_command(
          User.id(),
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) :: {:ok, map()} | error
  def edit_global_application_command(
        application_id \\ Me.get().id,
        command_id,
        command
      ) do
    Nostrum.Api.ApplicationCommand.edit_global_command(application_id, command_id, command)
  end

  @doc """
  Delete an existing global application command.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `command_id`: The current snowflake of the command.
  """
  @spec delete_global_application_command(Snowflake.t()) :: {:ok} | error
  @spec delete_global_application_command(User.id(), Snowflake.t()) :: {:ok} | error
  def delete_global_application_command(application_id \\ Me.get().id, command_id) do
    Nostrum.Api.ApplicationCommand.delete_global_command(application_id, command_id)
  end

  @doc """
  Overwrite the existing global application commands.

  This action will:
  - Create any command that was provided and did not already exist
  - Update any command that was provided and already existed if its configuration changed
  - Delete any command that was not provided but existed on Discord's end

  Updates will be available in all guilds after 1 hour.
  Commands that do not already exist will count toward daily application command create limits.

  ## Parameters
  - `application_id`: Application ID for which to overwrite the commands.
    If not given, this will be fetched from `Me`.
  - `commands`: List of command configurations, see the linked API documentation for reference.

  ## Return value
  Updated list of global application commands. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#bulk-overwrite-global-application-commands
  """
  @doc since: "0.5.0"
  @spec bulk_overwrite_global_application_commands([ApplicationCommand.application_command_map()]) ::
          {:ok, [map()]} | error
  @spec bulk_overwrite_global_application_commands(User.id(), [
          ApplicationCommand.application_command_map()
        ]) :: {:ok, [map()]} | error
  def bulk_overwrite_global_application_commands(application_id \\ Me.get().id, commands) do
    Nostrum.Api.ApplicationCommand.bulk_overwrite_global_commands(application_id, commands)
  end

  @doc """
  Fetch all guild application commands for the given guild.

  ## Parameters
  - `application_id`: Application ID for which to fetch commands.
    If not given, this will be fetched from `Me`.
  - `guild_id`: The guild ID for which guild application commands
    should be requested.

  ## Return value
  A list of ``ApplicationCommand``s on success. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure
  """
  @spec get_guild_application_commands(Guild.id()) :: {:ok, [map()]} | error
  @spec get_guild_application_commands(User.id(), Guild.id()) :: {:ok, [map()]} | error
  def get_guild_application_commands(application_id \\ Me.get().id, guild_id) do
    Nostrum.Api.ApplicationCommand.guild_commands(application_id, guild_id)
  end

  @doc """
  Create a guild application command on the specified guild.

  The new command will be available immediately.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild on which to create the command.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The created command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#create-guild-application-command
  """
  @spec create_guild_application_command(Guild.id(), ApplicationCommand.application_command_map()) ::
          {:ok, map()} | error
  @spec create_guild_application_command(
          User.id(),
          Guild.id(),
          ApplicationCommand.application_command_map()
        ) :: {:ok, map()} | error
  def create_guild_application_command(
        application_id \\ Me.get().id,
        guild_id,
        command
      ) do
    Nostrum.Api.ApplicationCommand.create_guild_command(application_id, guild_id, command)
  end

  @doc """
  Update an existing guild application command.

  The updated command will be available immediately.

  ## Parameters
  - `application_id`: Application ID for which to edit the command.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild for which to update the command.
  - `command_id`: The current snowflake of the command.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The updated command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#edit-guild-application-command
  """
  @spec edit_guild_application_command(
          Guild.id(),
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) :: {:ok, map()} | error
  @spec edit_guild_application_command(
          User.id(),
          Guild.id(),
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) ::
          {:ok, map()} | error
  def edit_guild_application_command(
        application_id \\ Me.get().id,
        guild_id,
        command_id,
        command
      ) do
    Nostrum.Api.ApplicationCommand.edit_guild_command(
      application_id,
      guild_id,
      command_id,
      command
    )
  end

  @doc """
  Delete an existing guild application command.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `guild_id`: The guild on which the command exists.
  - `command_id`: The current snowflake of the command.
  """
  @spec delete_guild_application_command(Guild.id(), Snowflake.t()) :: {:ok} | error
  @spec delete_guild_application_command(User.id(), Guild.id(), Snowflake.t()) :: {:ok} | error
  def delete_guild_application_command(
        application_id \\ Me.get().id,
        guild_id,
        command_id
      ) do
    Nostrum.Api.ApplicationCommand.delete_guild_command(application_id, guild_id, command_id)
  end

  @doc """
  Overwrite the existing guild application commands on the specified guild.

  This action will:
  - Create any command that was provided and did not already exist
  - Update any command that was provided and already existed if its configuration changed
  - Delete any command that was not provided but existed on Discord's end

  ## Parameters
  - `application_id`: Application ID for which to overwrite the commands.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild on which to overwrite the commands.
  - `commands`: List of command configurations, see the linked API documentation for reference.

  ## Return value
  Updated list of guild application commands. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#bulk-overwrite-guild-application-commands
  """
  @doc since: "0.5.0"
  @spec bulk_overwrite_guild_application_commands(Guild.id(), [
          ApplicationCommand.application_command_map()
        ]) :: {:ok, [map()]} | error
  @spec bulk_overwrite_guild_application_commands(User.id(), Guild.id(), [
          ApplicationCommand.application_command_map()
        ]) ::
          {:ok, [map()]} | error
  def bulk_overwrite_guild_application_commands(
        application_id \\ Me.get().id,
        guild_id,
        commands
      ) do
    Nostrum.Api.ApplicationCommand.bulk_overwrite_guild_commands(
      application_id,
      guild_id,
      commands
    )
  end

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
  @spec create_interaction_response!(Interaction.t(), map()) :: no_return() | {:ok}
  def create_interaction_response!(interaction, response) do
    create_interaction_response!(interaction.id, interaction.token, response)
  end

  @doc """
  Create a response to an interaction received from the gateway.

  ## Parameters
  - `id`: The interaction ID to which the response should be created.
  - `token`: The interaction token.
  - `response`: An [`InteractionResponse`](https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-response-object)
    object. See the linked documentation.


  ### Attachments
  To include attachments in the response, you can include a `:files` field in the response.
  This field expects a list of attachments which can be in either of the following formats:
  - A path to the file to upload.
  - A map with the following fields:
    - `:body` The file contents.
    - `:name` The filename of the file.

  ## Example

  ```elixir
  response = %{
    type: 4,
    data: %{
      content: "I copy and pasted this code."
    }
  }
  Nostrum.Api.create_interaction_response(interaction, response)
  ```

  As an alternative to passing the interaction ID and token, the
  original `t:Nostrum.Struct.Interaction.t/0` can also be passed
  directly. See `create_interaction_response/2`.
  """
  @spec create_interaction_response(Interaction.id(), Interaction.token(), map()) :: {:ok} | error
  def create_interaction_response(id, token, response) do
    request(
      :post,
      Constants.interaction_callback(id, token),
      combine_embeds(response) |> combine_files()
    )
  end

  def create_interaction_response!(id, token, response) do
    create_interaction_response(id, token, response)
    |> bangify
  end

  @doc """
  Retrieves the original message of an interaction.
  """
  @doc since: "0.7.0"
  @spec get_original_interaction_response(Interaction.t()) :: error | {:ok, Message.t()}
  def get_original_interaction_response(interaction) do
    path = Constants.original_interaction_response(interaction.application_id, interaction.token)

    request(:get, path)
    |> handle_request_with_decode({:struct, Message})
  end

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
  @spec edit_interaction_response!(Interaction.t(), map()) :: no_return() | Message.t()
  def edit_interaction_response!(%Interaction{} = interaction, response) do
    edit_interaction_response!(interaction.application_id, interaction.token, response)
  end

  @doc """
  Edits the original interaction response.

  Functions the same as `edit_webhook_message/3`
  """
  @doc since: "0.5.0"
  @spec edit_interaction_response(User.id(), Interaction.token(), map()) ::
          {:ok, Message.t()} | error
  def edit_interaction_response(id \\ Me.get().id, token, response) do
    request(
      :patch,
      Constants.interaction_callback_original(id, token),
      combine_embeds(response) |> combine_files()
    )
    |> handle_request_with_decode({:struct, Message})
  end

  @doc """
  Same as `edit_interaction_response/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
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
  @spec delete_interaction_response!(Interaction.t()) :: no_return() | {:ok}
  def delete_interaction_response!(%Interaction{} = interaction) do
    delete_interaction_response(interaction.application_id, interaction.token)
    |> bangify
  end

  @doc """
  Deletes the original interaction response.
  """
  @doc since: "0.5.0"
  @spec delete_interaction_response(User.id(), Interaction.token()) :: {:ok} | error
  def delete_interaction_response(id \\ Me.get().id, token) do
    request(:delete, Constants.interaction_callback_original(id, token))
  end

  @doc """
  Same as `delete_interaction_response/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @spec delete_interaction_response!(User.id(), Interaction.token()) :: no_return() | {:ok}
  def delete_interaction_response!(id \\ Me.get().id, token) do
    delete_interaction_response(id, token)
    |> bangify
  end

  @doc """
  Create a followup message for an interaction.

  Delegates to ``execute_webhook/3``, see the function for more details.
  """
  @spec create_followup_message(User.id(), Interaction.token(), map()) ::
          {:ok, Message.t()} | error
  def create_followup_message(application_id \\ Me.get().id, token, webhook_payload) do
    execute_webhook(application_id, token, webhook_payload)
  end

  @doc """
  Same as `create_followup_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
  @spec create_followup_message!(User.id(), Interaction.token(), map()) ::
          no_return() | Message.t()
  def create_followup_message!(application_id \\ Me.get().id, token, webhook_payload) do
    create_followup_message(application_id, token, webhook_payload)
    |> bangify
  end

  @doc """
  Delete a followup message for an interaction.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `token`: Interaction token.
  - `message_id`: Followup message ID.
  """
  @spec delete_interaction_followup_message(User.id(), Interaction.token(), Message.id()) ::
          {:ok} | error
  def delete_interaction_followup_message(
        application_id \\ Me.get().id,
        token,
        message_id
      ) do
    request(:delete, Constants.interaction_followup_message(application_id, token, message_id))
  end

  @doc """
  Same as `delete_interaction_followup_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @doc since: "0.5.0"
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

  @doc """
  Fetches command permissions for all commands for your application in a guild.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.

  ## Return value
  This method returns a list of guild application command permission objects, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "0.5.0"
  @spec get_guild_application_command_permissions(Guild.id()) :: {:ok, [map()]} | error
  @spec get_guild_application_command_permissions(User.id(), Guild.id()) :: {:ok, [map()]} | error
  def get_guild_application_command_permissions(
        application_id \\ Me.get().id,
        guild_id
      ) do
    Nostrum.Api.ApplicationCommand.guild_permissions(application_id, guild_id)
  end

  @doc """
  Fetches command permissions for a specific command for your application in a guild.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.
  - `command_id`: Command ID to fetch permissions for.

  ## Return value
  This method returns a single guild application command permission object, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "0.5.0"
  @spec get_application_command_permissions(Guild.id(), Snowflake.t()) ::
          {:ok, map()} | error
  @spec get_application_command_permissions(User.id(), Guild.id(), Snowflake.t()) ::
          {:ok, map()} | error
  def get_application_command_permissions(
        application_id \\ Me.get().id,
        guild_id,
        command_id
      ) do
    Nostrum.Api.ApplicationCommand.permissions(application_id, guild_id, command_id)
  end

  @doc """
  Edits command permissions for a specific command for your application in a guild. You can only add up to 10 permission overwrites for a command.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.
  - `command_id`: Command ID to fetch permissions for.
  - `permissions`: List of [application command permissions](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-application-command-permissions-structure)

  ## Return value
  This method returns a guild application command permission object, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "0.5.0"
  @spec edit_application_command_permissions(Guild.id(), Snowflake.t(), [
          ApplicationCommand.application_command_permissions()
        ]) ::
          {:ok, map()} | error
  @spec edit_application_command_permissions(User.id(), Guild.id(), Snowflake.t(), [
          ApplicationCommand.application_command_permissions()
        ]) ::
          {:ok, map()} | error
  def edit_application_command_permissions(
        application_id \\ Me.get().id,
        guild_id,
        command_id,
        permissions
      ) do
    Nostrum.Api.ApplicationCommand.edit_command_permissions(
      application_id,
      guild_id,
      command_id,
      permissions
    )
  end

  @doc """
  Edits command permissions for a specific command for your application in a guild. You can only add up to 10 permission overwrites for a command.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.
  - `command_id`: Command ID to fetch permissions for.
  - `permissions`: List of partial [guild application command permissions](hhttps://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure) with `id` and `permissions`. You can add up to 10 overwrites per command.

  ## Return value
  This method returns a guild application command permission object, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "0.5.0"
  @spec batch_edit_application_command_permissions(Guild.id(), [
          %{
            id: Snowflake.t(),
            permissions: [ApplicationCommand.application_command_permissions()]
          }
        ]) ::
          {:ok, map()} | error
  @spec batch_edit_application_command_permissions(User.id(), Guild.id(), [
          %{
            id: Snowflake.t(),
            permissions: [ApplicationCommand.application_command_permissions()]
          }
        ]) ::
          {:ok, map()} | error
  def batch_edit_application_command_permissions(
        application_id \\ Me.get().id,
        guild_id,
        permissions
      ) do
    Nostrum.Api.ApplicationCommand.batch_edit_permissions(
      application_id,
      guild_id,
      permissions
    )
  end

  @type thread_with_message_params :: %{
          required(:name) => String.t(),
          optional(:auto_archive_duration) => 60 | 1440 | 4320 | 10_080,
          optional(:rate_limit_per_user) => 0..21_600
        }

  @doc """
  Create a thread on a channel message.

  The `thread_id` will be the same as the id of the message, as such no message can have more than one thread.

  If successful, returns `{:ok, Channel}`. Otherwise returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options
  - `name`: Name of the thread, max 100 characters.
  - `auto_archive_duration`: Duration in minutes to auto-archive the thread after it has been inactive, can be set to 60, 1440, 4320, or 10080.
  - `rate_limit_per_user`: Rate limit per user in seconds, can be set to any value in `0..21600`.

  """
  @doc since: "0.5.1"
  @spec start_thread_with_message(
          Channel.id(),
          Message.id(),
          thread_with_message_params,
          AuditLogEntry.reason()
        ) ::
          {:ok, Channel.t()} | error
  def start_thread_with_message(channel_id, message_id, options, reason \\ nil) do
    Nostrum.Api.Thread.create_with_message(channel_id, message_id, options, reason)
  end

  @doc """
  Create a new thread in a forum channel.

  If successful, returns `{:ok, Channel}`. Otherwise returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options
  - `name`: Name of the thread, max 100 characters.
  - `auto_archive_duration`: Duration in minutes to auto-archive the thread after it has been inactive, can be set to 60, 1440, 4320, or 10080.
  - `rate_limit_per_user`: Rate limit per user in seconds, can be set to any value in `0..21600`.
  - `applied_tags`: An array of tag ids to apply to the thread.
  - `message`: The first message in the created thread.

  ### Thread Message Options
  - `content`: The content of the message.
  - `embeds`: A list of embeds.
  - `allowed_mentions`: Allowed mentions object.
  - `components`: A list of components.
  - `sticker_ids`: A list of sticker ids.
  - `:files` - a list of files where each element is the same format as the `:file` option. If both
    `:file` and `:files` are specified, `:file` will be prepended to the `:files` list.

  At least one of `content`, `embeds`, `sticker_ids`, or `files` must be specified.
  """
  @doc since: "0.7.0"
  @spec start_thread_in_forum_channel(Channel.id(), map(), AuditLogEntry.reason()) ::
          {:ok, Channel.t()} | error
  def start_thread_in_forum_channel(channel_id, options, reason \\ nil) do
    Nostrum.Api.Thread.create_in_forum(channel_id, options, reason)
  end

  @doc """
  Returns a thread member object for the specified user if they are a member of the thread
  """
  @doc since: "0.5.1"
  @spec get_thread_member(Channel.id(), User.id()) :: {:ok, ThreadMember.t()} | error
  def get_thread_member(thread_id, user_id) do
    Nostrum.Api.Thread.member(thread_id, user_id)
  end

  @doc """
  Returns a list of thread members for the specified thread.

  This endpoint is restricted according to whether the `GUILD_MEMBERS` privileged intent is enabled.
  """
  @doc since: "0.5.1"
  @spec get_thread_members(Channel.id()) :: {:ok, [ThreadMember.t()]} | error
  def get_thread_members(thread_id) do
    Nostrum.Api.Thread.members(thread_id)
  end

  @doc """
  Return all active threads for the current guild.

  Response body is a map with the following keys:
  - `threads`: A list of channel objects.
  - `members`: A list of `ThreadMember` objects, one for each returned thread the current user has joined.
  """
  @doc since: "0.5.1"
  @spec list_guild_threads(Guild.id()) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()]}} | error
  def list_guild_threads(guild_id) do
    Nostrum.Api.Thread.list(guild_id)
  end

  @doc """
  Returns a list of archived threads for a given channel.

  Threads are sorted by the `archive_timestamp` field, in descending order.

  ## Response body
  Response body is a map with the following keys:
  - `threads`: A list of channel objects.
  - `members`: A list of `ThreadMember` objects, one for each returned thread the current user has joined.
  - `has_more`: A boolean indicating whether there are more archived threads that can be fetched.

  ## Options
  - `before`: Returns threads before this timestamp, can be either a `DateTime` or [ISO8601 timestamp](`DateTime.to_iso8601/3`).
  - `limit`: Optional maximum number of threads to return.
  """
  @doc since: "0.5.1"
  @spec list_public_archived_threads(Channel.id(), options) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()], has_more: boolean()}}
          | error
  def list_public_archived_threads(channel_id, options \\ []) do
    Nostrum.Api.Thread.public_archived_threads(channel_id, options)
  end

  @doc """
  Same as `list_public_archived_threads/2`, but for private threads instead of public.
  """
  @doc since: "0.5.1"
  @spec list_private_archived_threads(Channel.id(), options) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()], has_more: boolean()}}
          | error
  def list_private_archived_threads(channel_id, options \\ []) do
    Nostrum.Api.Thread.private_archived_threads(channel_id, options)
  end

  @doc """
  Same as `list_public_archived_threads/2`, but only returns private threads that the current user has joined.
  """
  @doc since: "0.5.1"
  @spec list_joined_private_archived_threads(Channel.id(), options) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()], has_more: boolean()}}
          | error
  def list_joined_private_archived_threads(channel_id, options \\ []) do
    Nostrum.Api.Thread.joined_private_archived_threads(channel_id, options)
  end

  @doc """
  Join an existing thread, requires that the thread is not archived.
  """
  @doc since: "0.5.1"
  @spec join_thread(Channel.id()) :: {:ok} | error
  def join_thread(thread_id) do
    Nostrum.Api.Thread.join(thread_id)
  end

  @doc """
  Add a user to a thread, requires the ability to send messages in the thread.
  """
  @doc since: "0.5.1"
  def add_thread_member(thread_id, user_id) do
    Nostrum.Api.Thread.add_member(thread_id, user_id)
  end

  @doc """
  Leave a thread, requires that the thread is not archived.
  """
  @doc since: "0.5.1"
  @spec leave_thread(Channel.id()) :: {:ok} | error
  def leave_thread(thread_id) do
    Nostrum.Api.Thread.leave(thread_id)
  end

  @doc """
  Removes another user from a thread, requires that the thread is not archived.

  Also requires the `MANAGE_THREADS` permission, or the creator of the thread if the thread is private.
  """
  @doc since: "0.5.1"
  @spec remove_thread_member(Channel.id(), User.id()) :: {:ok} | error
  def remove_thread_member(thread_id, user_id) do
    Nostrum.Api.Thread.remove_member(thread_id, user_id)
  end

  @doc """
  Get a list of all auto-moderation rules for a guild.
  """
  @doc since: "0.7.0"
  @spec get_guild_auto_moderation_rules(Guild.id()) :: {:ok, [AutoModerationRule.t()]} | error
  def get_guild_auto_moderation_rules(guild_id) do
    Nostrum.Api.AutoModeration.rules(guild_id)
  end

  @doc """
  Get a single auto-moderation rule for a guild.
  """
  @doc since: "0.7.0"
  @spec get_guild_auto_moderation_rule(Guild.id(), AutoModerationRule.id()) ::
          {:ok, AutoModerationRule.t()} | error
  def get_guild_auto_moderation_rule(guild_id, rule_id) do
    Nostrum.Api.AutoModeration.rule(guild_id, rule_id)
  end

  @doc """
  Create a new auto-moderation rule for a guild.

  ## Options
    * `:name` (`t:String.t/0`) - The name of the rule.
    * `:event_type` (`t:AutoModerationRule.event_type/0`) - The type of event that triggers the rule.
    * `:trigger_type` (`t:AutoModerationRule.trigger_type/0`) - The type of content that triggers the rule.
    * `:trigger_metadata` (`t:AutoModerationRule.trigger_metadata/0`) - The metadata associated with the rule trigger.
      - optional, based on the `:trigger_type`.
    * `:actions` (`t:AutoModerationRule.actions/0`) - The actions to take when the rule is triggered.
    * `:enabled` (`t:AutoModerationRule.enabled/0`) - Whether the rule is enabled or not.
      - optional, defaults to `false`.
    * `:exempt_roles` - (`t:AutoModerationRule.exempt_roles/0`) - A list of role id's that are exempt from the rule.
      - optional, defaults to `[]`, maximum of 20.
    * `:exempt_channels` - (`t:AutoModerationRule.exempt_channels/0`) - A list of channel id's that are exempt from the rule.
      - optional, defaults to `[]`, maximum of 50.
  """
  @doc since: "0.7.0"
  @spec create_guild_auto_moderation_rule(Guild.id(), options()) ::
          {:ok, AutoModerationRule.t()} | error
  def create_guild_auto_moderation_rule(guild_id, options) when is_list(options),
    do: create_guild_auto_moderation_rule(guild_id, Map.new(options))

  def create_guild_auto_moderation_rule(guild_id, options) do
    Nostrum.Api.AutoModeration.create_rule(guild_id, options)
  end

  @doc """
  Modify an auto-moderation rule for a guild.

  Takes the same options as `create_guild_auto_moderation_rule/2`, however all fields are optional.
  """
  @doc since: "0.7.0"
  @spec modify_guild_auto_moderation_rule(Guild.id(), AutoModerationRule.id(), options()) ::
          {:ok, AutoModerationRule.t()} | error
  def modify_guild_auto_moderation_rule(guild_id, rule_id, options) when is_list(options),
    do: modify_guild_auto_moderation_rule(guild_id, rule_id, Map.new(options))

  def modify_guild_auto_moderation_rule(guild_id, rule_id, options) do
    Nostrum.Api.AutoModeration.modify_rule(guild_id, rule_id, options)
  end

  @doc """
  Delete an auto-moderation rule for a guild.
  """
  @doc since: "0.7.0"
  @spec delete_guild_auto_moderation_rule(Guild.id(), AutoModerationRule.id()) :: {:ok} | error
  def delete_guild_auto_moderation_rule(guild_id, rule_id) do
    Nostrum.Api.AutoModeration.delete_rule(guild_id, rule_id)
  end

  @spec maybe_add_reason(String.t() | nil) :: list()
  def maybe_add_reason(reason) do
    maybe_add_reason(reason, [{"content-type", "application/json"}])
  end

  @spec maybe_add_reason(String.t() | nil, list()) :: list()
  def maybe_add_reason(nil, headers) do
    headers
  end

  def maybe_add_reason(reason, headers) do
    [{"x-audit-log-reason", reason} | headers]
  end

  @spec request(map()) :: {:ok} | {:ok, String.t()} | error
  def request(request) do
    Ratelimiter.queue(request)
  end

  @spec request(atom(), String.t(), any, keyword() | map()) :: {:ok} | {:ok, String.t()} | error
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
    boundary = generate_boundary()
    {files, body} = combine_files(body) |> pop_files()
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

  # If `:embed` is present, prepend to `:embeds` for compatibility
  def combine_embeds(%{embed: embed} = args),
    do: Map.delete(args, :embed) |> Map.put(:embeds, [embed | args[:embeds] || []])

  def combine_embeds(%{data: data} = args), do: %{args | data: combine_embeds(data)}
  def combine_embeds(%{message: data} = args), do: %{args | message: combine_embeds(data)}
  def combine_embeds(args), do: args

  # If `:file` is present, prepend to `:files` for compatibility
  def combine_files(%{file: file} = args),
    do: Map.delete(args, :file) |> Map.put(:files, [file | args[:files] || []])

  def combine_files(%{data: data} = args), do: %{args | data: combine_files(data)}
  def combine_files(%{message: data} = args), do: %{args | message: combine_files(data)}
  def combine_files(args), do: args

  def pop_files(%{data: data} = args),
    do: {data.files, %{args | data: Map.delete(data, :files)}}

  def pop_files(%{message: data} = args),
    do: {data.files, %{args | message: Map.delete(data, :files)}}

  def pop_files(args), do: Map.pop!(args, :files)

  @doc false
  def bangify(to_bang) do
    Logger.warning("""
    Using the bangified version of the Api call is deprecated and will be removed in the next major release.
    Please use the non-bangified version and handle the error case yourself.
    """)

    case to_bang do
      {:error, error} ->
        raise(error)

      {:ok, body} ->
        body

      {:ok} ->
        {:ok}
    end
  end

  def prepare_allowed_mentions(options) do
    with raw_options when raw_options != :all <- Map.get(options, :allowed_mentions, :all),
         allowed_mentions when is_map(allowed_mentions) <- parse_allowed_mentions(raw_options) do
      Map.put(options, :allowed_mentions, allowed_mentions)
    else
      _ ->
        Map.delete(options, :allowed_mentions)
    end
  end

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

  defp create_file_part_for_multipart(file, index, boundary, name_override \\ nil) do
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

  def generate_boundary do
    String.duplicate("-", 20) <>
      "KraigieNostrumCat_" <>
      Base.encode16(:crypto.strong_rand_bytes(10))
  end

  defp parse_allowed_mentions(:none), do: %{parse: []}
  defp parse_allowed_mentions(:everyone), do: %{parse: [:everyone]}

  # Parse users
  defp parse_allowed_mentions(:users), do: %{parse: [:users]}
  defp parse_allowed_mentions({:users, users}) when is_list(users), do: %{users: users}

  # Parse roles
  defp parse_allowed_mentions(:roles), do: %{parse: [:roles]}
  defp parse_allowed_mentions({:roles, roles}) when is_list(roles), do: %{roles: roles}

  # Parse many
  defp parse_allowed_mentions(options) when is_list(options) or is_map(options) do
    options
    |> Enum.map(&parse_allowed_mentions/1)
    |> Enum.reduce(fn a, b ->
      Map.merge(a, b, fn
        key, parse_a, parse_b when key in [:parse, :users, :roles] ->
          Enum.uniq(parse_a ++ parse_b)

        _k, _v1, v2 ->
          v2
      end)
    end)
    |> Map.put_new(:parse, [])
  end

  # ignore
  defp parse_allowed_mentions(options), do: options

  @spec maybe_convert_date_time(options(), atom()) :: options()
  def maybe_convert_date_time(options, key) when is_map(options) do
    case options do
      %{^key => %DateTime{} = date_time} ->
        timestamp = DateTime.to_iso8601(date_time)
        %{options | key => timestamp}

      _ ->
        options
    end
  end

  def maybe_convert_date_time(options, key) when is_list(options) do
    case Keyword.get(options, key) do
      %DateTime{} = date_time ->
        timestamp = DateTime.to_iso8601(date_time)
        Keyword.put(options, key, timestamp)

      _ ->
        options
    end
  end
end
