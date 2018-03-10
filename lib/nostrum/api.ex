defmodule Nostrum.Api do
  @moduledoc ~S"""
  Interface for Discord's rest API.

  By default all methods in this module are ran synchronously. If you wish to
  have async rest operations I recommend you execute these functions inside of a
  task.

  **Examples**
  ```Elixir
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
  based off of an `id` in the response, you will need to conver it to an `int` using
  `String.to_integer/1`. I'm open to suggestions for how this should be handled going forward.

  **Example**
  ```Elixir
  messages = Nostrum.Api.get_pinned_messages!(12345678912345)

  authors =
    Enum.map messages, fn msg ->
      author_id = String.to_integer(msg.author.id)
      Nostrum.Cache.User.get!(id: author_id)
    end
  ```
  """

  use Bitwise

  alias Nostrum.{Constants, Util}
  alias Nostrum.Cache.Guild.GuildServer
  alias Nostrum.Struct.{Embed, Emoji, Guild, Message, User, Webhook}
  alias Nostrum.Struct.Guild.{Member, Channel, Role}
  alias Nostrum.Shard.{Supervisor, Session}

  @typedoc """
  Represents a message's content in the context of creating a message using the Api.
  """
  @type message_content :: String.t |
                           [content: String.t, embed: Embed.t] |
                           [file_name: String.t, file: String.t]

  @typedoc """
  Represents a failed response from the API.

  This occurs when hackney or HTTPoison fail, or when the API doesn't respond with `200` or `204`.
  """
  @type error :: {:error, Nostrum.Error.ApiError.t}

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
  @type locator :: {:before, integer} |
                   {:after, integer} |
                   {:around, integer} |
                   {}

  @typedoc """
  Represents different statuses the bot can have.

    - `:dnd` - Red circle.
    - `:idle` - Yellow circle.
    - `:online` - Green circle.
    - `:invisible` - The bot will appear offline.
  """
  @type status :: :dnd | :idle | :online | :invisible

  @doc """
  Updates the status of the bot for a certain shard.

  ## Parameters
    - `pid` - Pid of the shard.
    - `status` - Status of the bot.
    - `game` - The 'playing' text of the bot. Empty will clear.
    - `type` - The type of status to show. 0 (Playing) | 1 (Streaming) | 2 (Listening) | 3 (Watching)
    - `stream` - URL of twitch.tv stream
  """
  @spec update_shard_status(pid, status, String.t, integer, String.t) :: :ok
  def update_shard_status(pid, status, game, type \\ 0, stream \\ nil) do
    Session.update_status(pid, to_string(status), game, stream, type)
    :ok
  end

  @doc """
  Updates the status of the bot for all shards.

  See `update_shard_status/4` for usage.
  """
  @spec update_status(status, String.t, integer, String.t) :: :ok
  def update_status(status, game, type \\ 0, stream \\ nil) do
    Supervisor.update_status(status, game, stream, type)
    :ok
  end

  @doc ~S"""
  Send a message to a channel.

  ## Parameters
    - `channel_id` - Id of the channel to send the message to.
    - `content` - One of string, embed, or file to send to the channel.
    See `t:message_content/0` for more info.
    - `tts` - Whether the message should be read over text to speech.

  For the `channel_id` parameter, you can pass in a `Nostrum.Struct.Message`
  struct, and it will pull the id from there.

  ## Example
  ```Elixir
  Nostrum.Api.create_message(1111111111111, [content: "my os rules", file: ~S"C:\i\use\windows"])
  ```
  """
  @spec create_message(Channel.id | Message.t, message_content, boolean) :: error | {:ok, Message.t}
  def create_message(channel_id, content, tts \\ false)
  def create_message(%Message{channel_id: id}, content, tts) when is_binary(content),
    do: create_message(id, content, tts)
  
  # Sending regular messages
  def create_message(channel_id, content, tts) when is_binary(content), 
    do: do_create_message(channel_id, %{content: content, tts: tts})

  # Embeds
  def create_message(channel_id, [content: c, embed: e], tts), 
    do: do_create_message(channel_id, %{content: c, embed: e, tts: tts})
  
  # Files
  def create_message(channel_id, [file_name: c, file: f], tts), 
    do: do_create_message(channel_id, %{content: c, file: f, tts: tts})

  @doc """
  Same as `create_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_message!(Channel.id | Message.t, message_content, boolean) :: no_return | Message.t
  def create_message!(channel_id, content, tts \\ false) do
    create_message(channel_id, content, tts)
    |> bangify
  end

  @doc """
  Edit a message.

  ## Parameters
    - `message` - Message to edit.
    - `content` - New content of the message.
  """
  @spec edit_message(Message.t, String.t) :: error | {:ok, Message.t}
  def edit_message(%Message{id: id, channel_id: c_id}, content) do
    edit_message(c_id, id, content)
  end

  @doc """
  Edit a message.

  ## Parameters
    - `channel_id` - Id of the channel the message is in.
    - `message_id` - Id of the message to edit.
    - `content` - New content of the message.
  """
  @spec edit_message(Channel.id, Message.id, String.t) :: error | {:ok, Message.t}
  def edit_message(channel_id, message_id, content) do
    case request(:patch, Constants.channel_message(channel_id, message_id), %{content: content}) do 
      {:ok, body} -> 
        message =  
          body 
          |> Poison.decode!() 
          |> Util.cast({:struct, Message})
 
        {:ok, message} 
      other -> 
        other 
    end
  end

  @doc """
  Same as `edit_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec edit_message!(Message.t, String.t) :: error | {:ok, Message.t}
  def edit_message!(%Message{id: id, channel_id: c_id}, content) do
    edit_message(c_id, id, content)
    |> bangify
  end

  @doc """
  Same as `edit_message/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec edit_message!(Channel.id, Message.id, String.t) :: no_return | {:ok, Message.t}
  def edit_message!(channel_id, message_id, content) do
    edit_message(channel_id, message_id, content)
    |> bangify
  end

  @doc """
  Delete a message.

  ## Parameters
    - `message` - Message to delete.
  """
  @spec delete_message(Message.t) :: error | {:ok}
  def delete_message(%Message{id: id, channel_id: c_id}) do
    delete_message(c_id, id)
  end

  @doc """
  Delete a message.

  ## Parameters
    - `channel_id` - Id of the channel the message is in.
    - `message_id` - Id of the message to delete.
  """
  @spec delete_message(Channel.id, Message.id) :: error | {:ok}
  def delete_message(channel_id, message_id) do
    request(:delete, Constants.channel_message(channel_id, message_id))
  end

  @doc """
  Same as `delete_message/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_message!(Message.t) :: no_return | {:ok}
  def delete_message!(%Message{id: id, channel_id: c_id}) do
    delete_message(c_id, id)
    |> bangify
  end

  @doc """
  Same as `delete_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_message!(Channel.id, Message.id) :: no_return | {:ok}
  def delete_message!(channel_id, message_id) do
    delete_message(channel_id, message_id)
    |> bangify
  end

  @doc ~S"""
  Create a reaction for a message.


  Parameter `emoji` can be any of the following:

    * A `t:Nostrum.Struct.Emoji.emoji_api_name/0`.
    * A base 16 unicode emoji string.
    * A URI encoded string.

  ## Permissions

  This function requires that the nostrum user have the following permissions:

    * `READ_MESSAGE_HISTORY`
    * `ADD_REACTIONS` (if adding a new reaction)

  ## Examples

      iex> Nostrum.Api.create_reaction(123123123123, 321321321321, "\xF0\x9F\x98\x81")
      {:ok}
      iex> Nostrum.Api.create_reaction(123123123123, 321321321321, URI.encode("\u2b50"))
      {:ok}
  """
  @spec create_reaction(Channel.id, Message.id, String.t | Emoji.emoji_api_name) :: error | {:ok}
  def create_reaction(channel_id, message_id, emoji) do
    request(:put, Constants.channel_reaction_me(channel_id, message_id, emoji))
  end

  @doc """
  Same as `create_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_reaction!(Channel.id, Message.id, String.t | Emoji.emoji_api_name) :: no_return | {:ok}
  def create_reaction!(channel_id, message_id, emoji) do
    create_reaction(channel_id, message_id, emoji)
    |> bangify()
  end

  @doc """
  Deletes a reaction made by the user.

  Parameter `emoji` can be any of the following:

    * A `t:Nostrum.Struct.Emoji.emoji_api_name/0`.
    * A base 16 unicode emoji string.
    * A URI encoded string.

  ## Examples

      iex> Nostrum.Api.delete_own_reaction(123123123123, 321321321321, "\xF0\x9F\x98\x81")
      {:ok}
      iex> Nostrum.Api.delete_own_reaction(123123123123, 321321321321, URI.encode("\u2b50"))
      {:ok}
  """
  @spec delete_own_reaction(Channel.id, Message.id, String.t | Emoji.emoji_api_name) :: error | {:ok}
  def delete_own_reaction(channel_id, message_id, emoji) do
    request(:delete, Constants.channel_reaction_me(channel_id, message_id, emoji))
  end

  @doc """
  Same as `delete_own_reaction/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_own_reaction!(Channel.id, Message.id, String.t | Emoji.emoji_api_name) :: no_return | {:ok}
  def delete_own_reaction!(channel_id, message_id, emoji) do
    delete_own_reaction(channel_id, message_id, emoji)
    |> bangify()
  end

  @doc false
  @deprecated "Use `delete_user_reaction/4` instead"
  @spec delete_reaction(Channel.id, Message.id, String.t | Emoji.emoji_api_name, User.id) :: error | {:ok}
  def delete_reaction(channel_id, message_id, emoji, user_id), do: delete_user_reaction(channel_id, message_id, emoji, user_id)

  @doc """
  Gets all users who reacted with an `emoji`.

  Parameter `emoji` can be any of the following:

    * A `t:Nostrum.Struct.Emoji.emoji_api_name/0`.
    * A base 16 unicode emoji string.
    * A URI encoded string.

  If the request was successful, this function returns `{:ok, users}`, where 
  `users` is a list of `Nostrum.Struct.User`. Otherwise, this function 
  returns `{:error, reason}`.
  """
  @spec get_reactions(Channel.id, Message.id, String.t | Emoji.emoji_api_name) :: error | {:ok, [User.t]}
  def get_reactions(channel_id, message_id, emoji) do
    case request(:get, Constants.channel_reactions_get(channel_id, message_id, emoji)) do
      {:ok, body} ->
        users =
          body 
          |> Poison.decode!() 
          |> Enum.map(fn user_dto -> 
            User.to_struct(user_dto) 
          end) 
         
        {:ok, users}
      other ->
        other
    end
  end

  @doc """
  Same as `get_reactions/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_reactions!(Channel.id, Message.id, String.t | Emoji.custom_emoji) :: no_return | [User.t]
  def get_reactions!(channel_id, message_id, emoji) do
    get_reactions(channel_id, message_id, emoji)
    |> bangify
  end

  @doc """
  Deletes all reactions from a message.

  Reaction to delete is specified by
  `channel_id`, `message_id`, and `emoji`.

  Returns `{:ok}` if successful, `{:error, reason}` otherwise.
  """
  @spec delete_all_reactions(integer, integer) :: error | {:ok}
  def delete_all_reactions(channel_id, message_id) do
    request(:delete, Constants.channel_reactions_delete(channel_id, message_id))
  end

  @doc """
  Get a channel.

  Gets a channel specified by `id`.
  """
  @spec get_channel(Channel.id) :: error | {:ok, Channel.t}
  def get_channel(channel_id) do
    request(:get, Constants.channel(channel_id))
    |> handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Same as `get_channel/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_channel!(Channel.id) :: no_return | Channel.t
  def get_channel!(channel_id) do
    get_channel(channel_id)
    |> bangify
  end

  @doc """
  DEPRECATED

  Edit a channel.

  Edits a channel with `options`

  `options` is a kwl with the following optional keys:
   * `name` - New name of the channel.
   * `position` - Position of the channel.
   * `topic` - Topic of the channel. *Text Channels only*
   * `bitrate` - Bitrate of the voice channel. *Voice Channels only*
   * `user_limit` - User limit of the channel. 0 for no limit. *Voice Channels only*
  """
  @deprecated "Use `modify_channel/2` instead"
  @spec edit_channel(integer, [
      name: String.t,
      position: integer,
      topic: String.t,
      bitrate: String.t,
      user_limit: integer
    ]) :: error | {:ok, Nostrum.Struct.Channel.t}
  def edit_channel(channel_id, options) do
    case request(:patch, Constants.channel(channel_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  DEPRECATED

  Edit a channel.

  See `edit_channel/2` for parameters.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @deprecated "Use `edit_channel!/2` instead"
  @spec edit_channel!(integer, [
      name: String.t,
      position: integer,
      topic: String.t,
      bitrate: String.t,
      user_limit: integer
    ]) :: error | {:ok, Nostrum.Struct.Channel.t}
  def edit_channel!(channel_id, options) do
    edit_channel(channel_id, options)
    |> bangify
  end

  @doc """
  Delete a channel.

  Channel to delete is specified by `channel_id`.
  """
  @spec delete_channel(Channel.id()) :: error | {:ok, Channel.t()}
  def delete_channel(channel_id) do
    request(:delete, Constants.channel(channel_id))
    |> handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Same as `delete_channel/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_channel!(Channel.id()) :: no_return | Channel.t()
  def delete_channel!(channel_id) do
    delete_channel(channel_id)
    |> bangify
  end

  @doc """
  Retrieve messages from a channel.

  ## Parameters
    - `channel_id` - Id of the channel to get messages from.
    - `limit` - Number of messages to get.
    - `locator` - tuple indicating what messages you want to retrieve.
  """
  @spec get_channel_messages(Channel.id, limit, locator) :: error | {:ok, [Message.t]}
  def get_channel_messages(channel_id, limit, locator \\ {}) do
    get_messages_sync(channel_id, limit, [], locator)
  end

  defp get_messages_sync(channel_id, limit, messages, locator) when limit <= 100 do
    case get_channel_messages_call(channel_id, limit, locator) do
      {:ok, new_messages} -> {:ok, messages ++ new_messages}
      other -> other
    end
  end

  defp get_messages_sync(channel_id, limit, messages, locator) do
    case get_channel_messages_call(channel_id, 100, locator) do
      {:error, message} -> {:error, message}
      {:ok, []} -> {:ok, messages}
      {:ok, new_messages} ->
        new_limit = get_new_limit(limit, length(new_messages))
        new_locator = get_new_locator(locator, List.last(new_messages))
        get_messages_sync(channel_id, new_limit, messages ++ new_messages, new_locator)
    end
  end

  defp get_new_locator({}, last_message), do: {:before, last_message.id}
  defp get_new_locator(locator, last_message), do: put_elem(locator, 1, last_message.id)

  defp get_new_limit(:infinity, _new_message_count), do: :infinity
  defp get_new_limit(limit, message_count), do: limit - message_count

  # We're decoding the response at each call to catch any errors
  @doc false
  def get_channel_messages_call(channel_id, limit, locator) do
    qs_params =
      case locator do
        {} -> [{:limit, limit}]
        non_empty_locator -> [{:limit, limit}, non_empty_locator]
      end
    request(:get, Constants.channel_messages(channel_id), "", params: qs_params)
    |> handle_request_with_decode({:list, {:struct, Message}})
  end

  @doc """
  Retrieve messages from a channel.

  See `get_channel_message/3` for usage.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec get_channel_messages!(integer, limit, locator) :: no_return | [Message.t]
  def get_channel_messages!(channel_id, limit, locator) do
    get_channel_messages(channel_id, limit, locator)
    |> bangify
  end

  @doc """
  Retrieves a message from a channel.

  Message to retrieve is specified by `message_id` and `channel_id`.
  """
  @spec get_channel_message(integer, integer) :: error | {:ok, Message.t}
  def get_channel_message(channel_id, message_id) do
    request(:get, Constants.channel_message(channel_id, message_id))
    |> handle_request_with_decode({:struct, Message})
  end

  @doc """
  Retrieves a message from a channel.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec get_channel_message!(integer, integer) :: no_return | Message.t
  def get_channel_message!(channel_id, message_id) do
    get_channel_message(channel_id, message_id)
    |> bangify
  end

  @doc """
  Deletes multiple messages from a channel.

  `messages` is a list of `Nostrum.Struct.Message.id` that you wish to delete.

  This method can only delete messages sent within the last two weeks.
  `Filter` is an optional parameter that specifies whether messages sent over
  two weeks ago should be filtered out; defaults to `true`.
  """
  @spec bulk_delete_messages(integer, [Nostrum.Struct.Message.id], boolean) :: error | {:ok}
  def bulk_delete_messages(channel_id, messages, filter \\ true)
  def bulk_delete_messages(channel_id, messages, false), do:
    request(:post, Constants.channel_bulk_delete(channel_id), %{messages: messages})
  def bulk_delete_messages(channel_id, messages, true) do
    filter_before =
      ((Util.now() - 14 * 24 * 60 * 60) * 1000 - 1_420_070_400_000) <<< 22

    filtered_messages = Enum.filter(messages, fn message_id ->
      message_id > filter_before
    end)

    request(:post, Constants.channel_bulk_delete(channel_id), %{messages: filtered_messages})
  end

  @doc """
  Deletes multiple messages from a channel.

  See `bulk_delete_messages/2` for more info.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec bulk_delete_messages!(integer, [Nostrum.Struct.Message.id], boolean) :: no_return | {:ok}
  def bulk_delete_messages!(channel_id, messages, filter \\ true) do
    bulk_delete_messages(channel_id, messages, filter)
    |> bangify
  end

  @doc """
  Edit the permission overwrites for a user or role.

  Role or user to overwrite is specified by `channel_id` and `overwrite_id`.

  `permission_info` is a kwl with the following required keys:
   * `allow` - Bitwise value of allowed permissions.
   * `deny` - Bitwise value of denied permissions.
   * `type` - `member` if editing a user, `role` if editing a role.
  """
  @spec edit_channel_permissions(integer, integer, [
      allow: integer,
      deny: integer,
      type: String.t
    ]) :: error | {:ok}
  def edit_channel_permissions(channel_id, overwrite_id, permission_info) do
    request(:put, Constants.channel_permission(channel_id, overwrite_id), permission_info)
  end

  @doc """
  Edit the permission overwrites for a user or role.

  See `edit_channel_permissions/2` for more info.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec edit_channel_permissions!(integer, integer, [
      allow: integer,
      deny: integer,
      type: String.t
    ]) :: no_return | {:ok}
  def edit_channel_permissions!(channel_id, overwrite_id, permission_info) do
    edit_channel_permissions(channel_id, overwrite_id, permission_info)
    |> bangify
  end

  @doc """
  Delete a channel permission for a user or role.

  Role or user overwrite to delete is specified by `channel_id` and `overwrite_id`.
  """
  @spec delete_channel_permissions(integer, integer) :: error | {:ok}
  def delete_channel_permissions(channel_id, overwrite_id) do
    request(:delete, Constants.channel_permission(channel_id, overwrite_id))
  end

  @doc """
  Gets a list of invites for a channel.

  Channel to get invites for is specified by `channel_id`
  """
  @spec get_channel_invites(integer) :: error | {:ok, [Nostrum.Struct.Invite.t]}
  def get_channel_invites(channel_id) do
    case request(:get, Constants.channel_invites(channel_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Creates an invite for a channel.

  `options` is a kwl with the following optional keys:
   * `max_age` - Duration of invite in seconds before expiry, or 0 for never
   * `max_uses` - Max number of uses or 0 for unlimited.
   * `temporary` - Whether the invite should grant temporary membership.
   * `unique` - Used when creating unique one time use invites.
  """
  @spec create_channel_invite(integer, [
      max_age: integer,
      max_uses: integer,
      temporary: boolean,
      unique: boolean
    ]) :: error | {:ok, Nostrum.Struct.Invite.t}
  def create_channel_invite(channel_id, options \\ %{}) do
    case request(:post, Constants.channel_invites(channel_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Triggers the typing indicator.

  Triggers the typing indicator in the channel specified by `channel_id`.
  The typing indicator lasts for about 8 seconds and then automatically stops.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec start_typing(integer) :: error | {:ok}
  def start_typing(channel_id) do
    request(:post, Constants.channel_typing(channel_id))
  end


  @doc """
  Triggers the typing indicator.

  Triggers the typing indicator in the channel specified by `channel_id`.
  The typing indicator lasts for about 8 seconds and then automatically stops.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  @spec start_typing!(integer) :: no_return | {:ok}
  def start_typing!(channel_id) do
    start_typing(channel_id)
    |> bangify
  end

  @doc """
  Gets all pinned messages.

  Retrieves all pinned messages for the channel specified by `channel_id`.

  Returns {:ok, [Message.t]} if successful. `error` otherwise.
  """
  @spec get_pinned_messages(integer) :: error | {:ok, [Message.t]}
  def get_pinned_messages(channel_id) do
    request(:get, Constants.channel_pins(channel_id))
    |> handle_request_with_decode({:list, {:struct, Message}})
  end

  @doc """
  Gets all pinned messages.

  Retrieves all pinned messages for the channel specified by `channel_id`.

  Returns [Message.t] if successful. `error` otherwise.
  """
  @spec get_pinned_messages!(integer) :: no_return | [Message.t]
  def get_pinned_messages!(channel_id) do
    get_pinned_messages(channel_id)
    |> bangify
  end

  @doc """
  Pins a message.

  Pins the message specified by `message_id` in the channel specified by `channel_id`.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec add_pinned_message(integer, integer) :: error | {:ok}
  def add_pinned_message(channel_id, message_id) do
    request(:put, Constants.channel_pin(channel_id, message_id))
  end

  @doc """
  Pins a message.

  Pins the message specified by `message_id` in the channel specified by `channel_id`.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  @spec add_pinned_message!(integer, integer) :: no_return | {:ok}
  def add_pinned_message!(channel_id, message_id) do
    add_pinned_message(channel_id, message_id)
    |> bangify
  end

  @doc """
  Unpins a message.

  Unpins the message specified by `message_id` in the channel specified by `channel_id`.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec delete_pinned_message(integer, integer) :: error | {:ok}
  def delete_pinned_message(channel_id, message_id) do
    request(:delete, Constants.channel_pin(channel_id, message_id))
  end

  @doc """
  Unpins a message.

  Unpins the message specified by `message_id` in the channel specified by `channel_id`.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  @spec delete_pinned_message!(integer, integer) :: no_return | {:ok}
  def delete_pinned_message!(channel_id, message_id) do
    delete_pinned_message(channel_id, message_id)
    |> bangify
  end

  @doc """
  Gets a guild using the REST api

  Retrieves a guild with specified `guild_id`.

  Returns {:ok, Nostrum.Struct.Guild.t} if successful, `error` otherwise.
  """
  @spec get_guild(integer) :: error | {:ok, Nostrum.Struct.Guild.t}
  def get_guild(guild_id) do
    request(:get, Constants.guild(guild_id))
    |> handle(Guild)
  end

  @doc """
  Gets a guild using the REST api

  Retrieves a guild with specified `guild_id`.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  Returns `Nostrum.Struct.Guild.t` if successful.
  """
  @spec get_guild!(integer) :: no_return | Nostrum.Struct.Guild.t
  def get_guild!(guild_id) do
    get_guild(guild_id)
    |> bangify
  end

  @doc """
  Modify a guild's settings.

  `options` is a map with the following optional keys:
   * `name` - Guild name.
   * `region` - Guild voice region id.
   * `verification_level` - Guild verification level.
   * `default_message_notifications` - Notifications setting.
   * `afk_channel_id` - Id for afk channel.
   * `afk_timeout` - Afk timeout in seconds.
   * `icon` - Base64 encoded 128x128 jpeg image for guild icon.
   * `owner_id` - User id to transfer guild ownership to.
   * `splash` - Base64 encoded 128x128 jpeg image for guild splash.
  """
  @spec edit_guild(integer, %{
      name: String.t,
      region: String.t,
      verification_level: integer,
      default_message_notifications: boolean,
      afk_channel_id: integer,
      afk_timeout: integer,
      icon: String.t,
      owner_id: integer,
      splash: String.t
    }) :: error | {:ok, Nostrum.Struct.Guild.t}
  def edit_guild(guild_id, options) do
    case request(:patch, Constants.guild(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Deletes a guild.

  Guild to delete specified by `guild_id`.
  """
  @spec delete_guild(integer) :: error | {:ok}
  def delete_guild(guild_id) do
    request(:delete, Constants.guild(guild_id))
  end

  @doc """
  DEPRECATED

  Gets a list of channels.

  Guild to get channels for is specified by `guild_id`.
  """
  @deprecated "Use `get_guild_channels/1` instead"
  @spec get_channels(integer) :: error | {:ok, Nostrum.Struct.Channel.t}
  def get_channels(guild_id) do
    case request(:get, Constants.guild_channels(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  DEPRECATED

  Creates a channel.

  Guild to create channel in is specifed by `guild_id`.

  `options` is a map with the following optional keys (except for name):
   * `name` - Channel name.
   * `type` - Channel type.
   * `bitrate` - Bitrate if creating voice channel.
   * `user_limit` - User limit if creating voice channel.
   * `permission_overwrites` - Array of permission overwrites.
  """
  @deprecated "Use `create_guild_channel/2` instead"
  @spec create_channel(integer, %{
      name: String.t,
      type: String.t,
      bitrate: integer,
      user_limit: integer,
      permission_overwrites: [Nostrum.Struct.Overwrite.t]
    }) :: error | {:ok, Nostrum.Struct.Channel.t}
  def create_channel(guild_id, options) do
    case request(:post, Constants.guild_channels(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """ 
  Modify the positions of a set of channels for the guild. 
 
  ## Request Params
 
  Unlike other request params, this function's params accepts a list of maps.
   
  Each of these maps require the following keys: 
   
    * `:id` (integer) - channel id 
    * `:position` (integer) - sorting position of the channel 
 
  ## Examples 
 
      Nostrum.Api.modify_guild_channel_positions(41771983423143937, [%{id: 41771983423143936, position: 2}])

  """ 
  @spec modify_guild_channel_positions(Guild.id(), [map]) :: error | {:ok}
  def modify_guild_channel_positions(guild_id, params) when is_list(params) do
    request(:patch, Constants.guild_channels(guild_id), params)
  end

  @doc false
  @deprecated "Use `get_guild_member/2` instead"
  @spec get_member(integer, integer) :: error | {:ok, Nostrum.Struct.Guild.Member.t}
  def get_member(guild_id, user_id) do
    case request(:get, Constants.guild_member(guild_id, user_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc false
  @deprecated "Use `list_guild_members/2` instead"
  @spec get_guild_members(Guild.id, %{
    limit: 1..1000,
    after: integer
  }) :: error | {:ok, [Nostrum.Struct.Guild.Member.t]}
  def get_guild_members(guild_id, options) do
    request(:get, Constants.guild_members(guild_id), "", params: options)
    |> handle([Member])
  end

  @doc false
  @deprecated "Use `add_guild_member/3` instead"
  @spec add_member(integer, integer, %{
      nick: String.t,
      roles: [integer],
      mute: boolean,
      deaf: boolean,
      channel_id: integer
    }) :: error | {:ok, Nostrum.Struct.Guild.Member.t}
  def add_member(guild_id, user_id, options) do
    case request(:put, Constants.guild_member(guild_id, user_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc false
  @deprecated "Use `modify_guild_member/3` instead"
  @spec modify_member(integer, integer, %{
      nick: String.t,
      roles: [integer],
      mute: boolean,
      deaf: boolean,
      channel_id: integer
    }) :: error | {:ok, Nostrum.Struct.Guild.Member.t}
  def modify_member(guild_id, user_id, options) do
    request(:patch, Constants.guild_member(guild_id, user_id), options)
  end

  @doc """
  Adds a role to a member.

  Role to add is specified by `role_id`.
  User to add role to is specified by `guild_id` and `user_id`.
  """
  @spec add_guild_member_role(integer, integer, integer) :: error | {:ok}
  def add_guild_member_role(guild_id, user_id, role_id) do
    request(:put, Constants.guild_member_role(guild_id, user_id, role_id))
  end

  @doc """
  Removes a role from a member.

  Role to remove is specified by `role_id`.
  User to remove role from is specified by `guild_id` and `user_id`.
  """
  @spec remove_guild_member_role(integer, integer, integer) :: error | {:ok}
  def remove_guild_member_role(guild_id, user_id, role_id) do
    request(:delete, Constants.guild_member_role(guild_id, user_id, role_id))
  end

  @doc false
  @deprecated "Use `remove_guild_member/2` instead"
  @spec remove_member(integer, integer) :: error | {:ok}
  def remove_member(guild_id, user_id) do
    request(:delete, Constants.guild_member(guild_id, user_id))
  end

  @doc """
  Gets a list of users banend from a guild.

  Guild to get bans for is specified by `guild_id`.
  """
  @spec get_guild_bans(integer) :: error | {:ok, [Nostrum.Struct.User.t]}
  def get_guild_bans(guild_id) do
    case request(:get, Constants.guild_bans(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Bans a user from a guild.

  User to delete is specified by `guild_id` and `user_id`.
  """
  @spec create_guild_ban(integer, integer, integer) :: error | {:ok}
  def create_guild_ban(guild_id, user_id, days_to_delete) do
    request(:put, Constants.guild_ban(guild_id, user_id), %{"delete-message-days": days_to_delete})
  end

  @doc """
  Removes a ban for a user.

  User to unban is specified by `guild_id` and `user_id`.
  """
  @spec remove_guild_ban(integer, integer) :: error | {:ok}
  def remove_guild_ban(guild_id, user_id) do
    request(:remove, Constants.guild_ban(guild_id, user_id))
  end

  @doc """
  Gets a guild's roles.

  Guild to get roles for is specified by `guild_id`.
  """
  @spec get_guild_roles(Guild.id) :: error | {:ok, [Role.t]}
  def get_guild_roles(guild_id) do
    request(:get, Constants.guild_roles(guild_id))
    |> handle_request_with_decode({:list, {:struct, Role}})
  end

  @doc """
  Same as `get_guild_roles/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_roles!(Guild.id) :: no_return | [Role.t]
  def get_guild_roles!(guild_id) do
    get_guild_roles(guild_id)
    |> bangify()
  end

  @doc """
  Creates a guild role.

  ## Request Params
 
  The following params are optional:
 
    * `:name` (string) - name of the role (default: "new role")
    * `:permissions` (integer) - bitwise of the enabled/disabled permissions (default: @everyone perms)
    * `:color` (integer) - RGB color value (default: 0)
    * `:hoist` (boolean) - whether the role should be displayed separately in the sidebar (default: false)
    * `:mentionable` (boolean) - whether the role should be mentionable (default: false)
 
  ## Examples
 
      iex> Nostrum.Api.create_guild_role(41771983423143937, name: "nostrum-club", hoist: true)
      {:ok, %Nostrum.Struct.Guild.Role{}}
  """
  @spec create_guild_role(Guild.id, keyword | map) :: error | {:ok, Role.t}
  def create_guild_role(guild_id, params)
  def create_guild_role(guild_id, params) when is_list(params),
    do: create_guild_role(guild_id, Map.new(params))
  
  def create_guild_role(guild_id, %{} = params) do
    request(:post, Constants.guild_roles(guild_id), params)
    |> handle_request_with_decode({:struct, Role})
  end

  @doc """
  Same as `create_guild_role/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_role!(Guild.id, keyword | map) :: no_return | Role.t
  def create_guild_role!(guild_id, params) do
    create_guild_role(guild_id, params)
    |> bangify()
  end

  @doc """
  Reorders a guild's roles.

  ## Request Params
 
  Unlike other params, this function's params accepts a list of maps. Each of these 
  maps require the following keys:
  
    * `:id` (integer) - role
    * `:position` (integer) - sorting position of the role
 
  ## Examples
 
      Nostrum.Api.modify_guild_role_positions(41771983423143937, [%{id: 41771983423143936, position: 2}])
  
  """
  @spec modify_guild_role_positions(Guild.id, [%{
      id: integer,
      position: integer
    }]) :: error | {:ok, [Role.t]}
  def modify_guild_role_positions(guild_id, params) do
    request(:patch, Constants.guild_roles(guild_id), params)
    |> handle_request_with_decode({:list, {:struct, Role}})
  end

  @doc """
  Same as `modify_guild_role_positions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_role_positions!(Guild.id, [%{
    id: integer,
    position: integer
  }]) :: no_return | [Role.t]
  def modify_guild_role_positions!(guild_id, params) do
    modify_guild_role_positions(guild_id, params)
    |> bangify()
  end

  @doc """
  Modifies a guild role.
  
  ## Request Params 
 
  The following keys are optional: 
 
    * `:name` (string) - name of the role. 
    - `:permissions` (integer) - bitwise of the enabled/disabled permissions. 
    - `:color` (integer) - RGB color value. 
    - `:hoist` (boolean) - whether the role should be displayed seperately in the sidebar. 
    - `:mentionable` (boolean) - whether the role should be mentionable. 
 
  ## Examples 
 
      Nostrum.Api.modify_guild_role(41771983423143937, 41771983423143936, hoist: false)

      Nostrum.Api.modify_guild_role(41771983423143937, 41771983423143936, %{hoist: false})
  """ 
  @spec modify_guild_role(Guild.id, Role.id, keyword | map) :: error | {:ok, Role.t}
  def modify_guild_role(guild_id, role_id, params)
  def modify_guild_role(guild_id, role_id, params) when is_list(params), 
    do: modify_guild_role(guild_id, role_id, Map.new(params))

  def modify_guild_role(guild_id, role_id, %{} = params) do
    request(:patch, Constants.guild_role(guild_id, role_id), params)
    |> handle_request_with_decode({:struct, Role})
  end

  @doc """
  Same as `modify_guild_role/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_role!(Guild.id, Role.id, keyword | map) :: no_return | Role.t
  def modify_guild_role!(guild_id, role_id, params) do
    modify_guild_role(guild_id, role_id, params)
    |> bangify()
  end

  @doc """
  Deletes a guild role.

  ## Examples 
 
      Nostrum.Api.delete_guild_role(41771983423143937, 41771983423143936)
  """
  @spec delete_guild_role(Guild.id, Role.id) :: error | {:ok}
  def delete_guild_role(guild_id, role_id) do
    request(:delete, Constants.guild_role(guild_id, role_id))
  end

  @doc """
  Gets the number of members that would be removed in a prune.

  Guild to get prune number for is specified by guild_id.
  Days is that number of days to count prune for.
  """
  @spec get_guild_prune(integer, integer) :: error | {:ok, %{pruned: integer}}
  def get_guild_prune(guild_id, options) do
    case request(:get, Constants.guild_prune(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Begins a guild prune.

  Guild to number is specified by guild_id.
  Days is that number of days to count prune for.
  """
  @spec begin_guild_prune(integer, integer) :: error | {:ok, %{pruned: integer}}
  def begin_guild_prune(guild_id, days) do
    case request(:post, Constants.guild_prune(guild_id), "", params: [days: days]) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a list of voice regions for the guild.

  Guild to get voice regions for is specified by `guild_id`.
  """
  @spec get_voice_region(integer) :: error | {:ok, [Nostrum.Struct.VoiceRegion.t]}
  def get_voice_region(guild_id) do
    case request(:get, Constants.guild_voice_regions(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a list of invites for a guild.

  Guilds to get voice regions for is specified by `guild_id`.
  """
  @spec get_guild_invites(integer) :: error | {:ok, [Nostrum.Struct.Invite.t]}
  def get_guild_invites(guild_id) do
    case request(:get, Constants.guild_invites(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a list of guild integerations.

  Guild to get integrations for is specified by `guild_id`.
  """
  @spec get_guild_integrations(integer) :: error | {:ok, [Nostrum.Struct.Guild.Integration.t]}
  def get_guild_integrations(guild_id) do
    case request(:get, Constants.guild_integrations(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Creates a new guild integeration.

  Guild to create integration with is specified by `guild_id`.

  `options` is a map with the following requires keys:
   * `type` - Integration type.
   * `id` - Integeration id.
  """
  @spec create_guild_integrations(integer, %{
      type: String.t,
      id: integer
    }) :: error | {:ok}
  def create_guild_integrations(guild_id, options) do
    request(:post, Constants.guild_integrations(guild_id), options)
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
    request(:patch, Constants.guild_integration(guild_id, integration_id), options)
  end

  @doc """
  Deletes a guild integeration.

  Integration to delete is specified by `guild_id` and `integeration_id`.
  """
  @spec delete_guild_integrations(integer, integer) :: error | {:ok}
  def delete_guild_integrations(guild_id, integration_id) do
    request(:delete, Constants.guild_integration(guild_id, integration_id))
  end

  @doc """
  Syncs a guild integration.

  Integration to sync is specified by `guild_id` and `integeration_id`.
  """
  @spec sync_guild_integrations(integer, integer) :: error | {:ok}
  def sync_guild_integrations(guild_id, integration_id) do
    request(:post, Constants.guild_integration_sync(guild_id, integration_id))
  end

  @doc """
  Gets a guild embed.
  """
  @spec get_guild_embed(integer) :: error | {:ok, map}
  def get_guild_embed(guild_id) do
    request(:get, Constants.guild_embed(guild_id))
  end

  @doc """
  Modifies a guild imbed.
  """
  @spec modify_guild_embed(integer, map) :: error | {:ok, map}
  def modify_guild_embed(guild_id, options) do
    case request(:patch, Constants.guild_embed(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets an invite.

  Invite to get is specified by `invite_code`.
  """
  @spec get_invite(integer) :: error | {:ok, Nostrum.Struct.Invite.t}
  def get_invite(invite_code) do
    case request(:get, Constants.invite(invite_code)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Deletes an invite.

  Invite to delete is specified by `invite_code`.
  """
  @spec delete_invite(integer) :: error | {:ok, Nostrum.Struct.Invite.t}
  def delete_invite(invite_code) do
    case request(:delete, Constants.invite(invite_code)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Accepts an invite.

  Not available to bot accounts. Invite to accept is specified by `invite_code`.
  """
  @spec accept_invite(integer) :: error | {:ok, Nostrum.Struct.Invite.t}
  def accept_invite(invite_code) do
    request(:post, Constants.invite(invite_code))
  end

  @doc """
  Gets a user by its `user_id`.

  If the request is successful, this function returns `{:ok, user}`, where 
  `user` is a `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get_user(User.id) :: error | {:ok, User.t}
  def get_user(user_id) do
    case request(:get, Constants.user(user_id)) do 
      {:ok, body} -> 
        user =  
          body 
          |> Poison.decode!() 
          |> User.to_struct() 
 
        {:ok, user} 
      other -> 
        other 
    end 
  end

  @doc """
  Same as `get_user/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_user!(User.id) :: no_return | User.t
  def get_user!(user_id) do
    get_user(user_id)
    |> bangify
  end

  @doc """
  Gets info on the current user.

  If nostrum's caching is enabled, it is recommended to use `Nostrum.Cache.Me.get/0` 
  instead of this function. This is because sending out an API request is much slower 
  than pulling from our cache.

  If the request is successful, this function returns `{:ok, user}`, where 
  `user` is nostrum's `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get_current_user() :: error | {:ok, User.t}
  def get_current_user do
    case request(:get, Constants.me) do
      {:ok, body} ->
        user =  
          body 
          |> Poison.decode!() 
          |> User.to_struct() 
 
        {:ok, user}
      other ->
        other
    end
  end

  @doc """
  Same as `get_current_user/0`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_current_user!() :: no_return | User.t
  def get_current_user! do
    get_current_user()
    |> bangify
  end

  @doc ~S"""
  Changes the username or avatar of the current user.

  ## Params 
 
  The following params are optional: 
 
    * `:username` (string) - new username
    * `:avatar` (string) - the user's avatar as [avatar data](https://discordapp.com/developers/docs/resources/user#avatar-data)
 
  ## Examples 

      iex> Nostrum.Api.modify_current_user(avatar: "data:image/jpeg;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8=") 
      {:ok, %Nostrum.Struct.User{}}
  """
  @spec modify_current_user(keyword | map) :: error | {:ok, User.t}
  def modify_current_user(params)
  def modify_current_user(params) when is_list(params), do: modify_current_user(Map.new(params))

  def modify_current_user(%{} = params) do
    case request(:patch, Constants.me, params) do
      {:ok, body} ->
        user =  
          body 
          |> Poison.decode!() 
          |> User.to_struct() 
 
        {:ok, user}
      other ->
        other
    end
  end

  @doc """
  Same as `modify_current_user/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_current_user!(keyword | map) :: no_return | User.t
  def modify_current_user!(params) do
    modify_current_user(params)
    |> bangify()
  end

  @doc """
  Gets a list of guilds the user is currently in.

  `options` is an optional map with the following optional keys:
   * `before` - Get guilds before this ID.
   * `after` - Get guilds after this ID.
   * `limit` - Max number of guilds to return.
  """
  @spec get_current_users_guilds(%{
    before: integer,
    after: integer,
    limit: integer
    }) :: error | {:ok, [Nostrum.Struct.Guild.t]}
  def get_current_users_guilds(options \\ %{}) do
    case request(:get, Constants.me_guilds, "", params: options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Leaves a guild.

  Guild to leave is specified by `guild_id`.
  """
  @spec leave_guild(integer) :: error | {:ok}
  def leave_guild(guild_id) do
    request(:delete, Constants.me_guild(guild_id))
  end

  @doc """
  Gets a list of user DM channels.
  """
  @spec get_user_dms() :: error | {:ok, [Channel.t()]}
  def get_user_dms do
    request(:get, Constants.me_channels)
    |> handle_request_with_decode({:list, {:struct, Channel}})
  end

  @doc """
  Creates a new DM channel.

  Opens a DM channel with the user specified by `user_id`.
  """
  @spec create_dm(integer) :: error | {:ok, Nostrum.Struct.DMChannel.t}
  def create_dm(user_id) do
    case request(:post, Constants.me_channels, %{recipient_id: user_id}) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

    @doc """
    Creates a new group DM channel.
    """
    @spec create_group_dm([String.t], map) :: error | {:ok, Nostrum.Struct.DMChannel.t}
    def create_group_dm(access_tokens, nicks) do
      case request(:post, Constants.me_channels, %{access_tokens: access_tokens, nicks: nicks}) do
        {:ok, body} ->
          {:ok, Poison.decode!(body)}
        other ->
          other
      end
    end

  @doc """
  Gets a list of user connections.
  """
  @spec get_user_connections() :: error | {:ok, Nostrum.Struct.User.Connection.t}
  def get_user_connections do
    case request(:get, Constants.me_connections) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a list of voice regions.
  """
  @spec list_voice_regions() :: error | {:ok, [Nostrum.Struct.VoiceRegion.t]}
  def list_voice_regions do
    case request(:get, Constants.regions) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Creates a webhook.

  ## Parameters
    - `channel_id` - Id of the channel to send the message to.
    - `args` - Map with the following **required** keys:
      - `name` - Name of the webhook.
      - `avatar` - Base64 128x128 jpeg image for the default avatar.
  """
  @spec create_webhook(Channel.id, %{
      name: String.t,
      avatar: String.t
    }) :: error | {:ok, Nostrum.Struct.Webhook.t}
  def create_webhook(channel_id, args) do
    case request(:post, Constants.webhooks_channel(channel_id), args) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a list of webook for a channel.

  ## Parameters
    - `channel_id` - Channel to get webhooks for.
  """
  @spec get_channel_webhooks(Channel.id)
    :: error | {:ok, [Nostrum.Struct.Webhook.t]}
  def get_channel_webhooks(channel_id) do
    case request(:get, Constants.webhooks_channel(channel_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a list of webooks for a guild.

  ## Parameters
    - `guild_id` - Guild to get webhooks for.
  """
  @spec get_guild_webhooks(Guild.id) :: error | {:ok, [Nostrum.Struct.Webhook.t]}
  def get_guild_webhooks(guild_id) do
    case request(:get, Constants.webhooks_guild(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a webhook by id.

  ## Parameters
    - `webhook_id` - Id of webhook to get.
  """
  @spec get_webhook(Webhook.id) :: error | {:ok, Nostrum.Struct.Webhook.t}
  def get_webhook(webhook_id) do
    case request(:get, Constants.webhook(webhook_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Gets a webhook by id and token.

  This method is exactly like `get_webhook/1` but does not require
  authentication.

  ## Parameters
    - `webhook_id` - Id of webhook to get.
    - `webhook_token` - Token of the webhook to get.
  """
  @spec get_webhook_with_token(Webhook.id, Webhook.token) :: error | {:ok, Nostrum.Struct.Webhook.t}
  def get_webhook_with_token(webhook_id, webhook_token) do
    case request(:get, Constants.webhook_token(webhook_id, webhook_token)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Modifies a webhook.

  ## Parameters
    - `webhook_id` - Id of the webhook to modify.
    - `args` - Map with the following *optional* keys:
      - `name` - Name of the webhook.
      - `avatar` - Base64 128x128 jpeg image for the default avatar.
  """
  @spec modify_webhook(Webhook.id, %{
      name: String.t,
      avatar: String.t
    }) :: error | {:ok, Nostrum.Struct.Webhook.t}
  def modify_webhook(webhook_id, args) do
    case request(:patch, Constants.webhook(webhook_id), args) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
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
  """
  @spec modify_webhook_with_token(Webhook.id, Webhook.token, %{
      name: String.t,
      avatar: String.t
    }) :: error | {:ok, Nostrum.Struct.Webhook.t}
  def modify_webhook_with_token(webhook_id, webhook_token, args) do
    case request(:patch, Constants.webhook_token(webhook_id, webhook_token), args) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Deletes a webhook.

  ## Parameters
    - `webhook_id` - Id of webhook to delete.
    - `webhook_token` - Token of the webhook to delete.
  """
  @spec delete_webhook(Webhook.id) :: error | {:ok}
  def delete_webhook(webhook_id) do
    request(:delete, Constants.webhook(webhook_id))
  end

  @doc """
  Executes a webhook.

  ## Parameters
  - `webhook_id` - Id of webhook to delete.
  - `webhook_token` - Token of the webhook to delete.
  - `args` - Map with the following required keys:
    - `content` - Message content.
    - `file` - File to send.
    - `embeds` - Embed to send.
    - `username` - Overrides the default name of the webhook.
    - `avatar_url` - Overrides the default avatar of the webhook.
    - `tts` - Whether the message should be read over text to speech.
  - `wait` - Whether to return an error or not. Defaults to `false`.

  Only one of `content`, `file` or `embed` should be supplied in the `args` parameter.
  """
  @spec execute_webhook(Webhook.id, Webhook.token, %{
      content: String.t,
      username: String.t,
      avatar_url: String.t,
      tts: boolean,
      file: String.t,
      embeds: Embed.t
    }, boolean) :: error | {:ok}
  def execute_webhook(webhook_id, webhook_token, args, wait \\ false)
  def execute_webhook(webhook_id, webhook_token, %{file: _} = args, wait) do
    case \
    request_multipart(
      :post,
      Constants.webhook_token(webhook_id, webhook_token),
      args,
      params: [wait: wait]
    ) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  def execute_webhook(webhook_id, webhook_token, %{content: _} = args, wait) do
    case \
    request(
      :post,
      Constants.webhook_token(webhook_id, webhook_token),
      args,
      params: [wait: wait]
    ) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}
      other ->
        other
    end
  end

  @doc """
  Executes a slack webhook.

  ## Parameters
    - `webhook_id` - Id of webhook to delete.
    - `webhook_token` - Token of the webhook to delete.
  """
  @spec execute_slack_webhook(Webhook.id, Webhook.token, boolean) :: error | {:ok}
  def execute_slack_webhook(webhook_id, webhook_token, wait \\ false) do
    request(:post, Constants.webhook_slack(webhook_id, webhook_token), params: [wait: wait])
  end

  @doc """
  Executes a git webhook.

  ## Parameters
    - `webhook_id` - Id of webhook to delete.
    - `webhook_token` - Token of the webhook to delete.
  """
  @spec execute_git_webhook(Webhook.id, Webhook.token, boolean) :: error | {:ok}
  def execute_git_webhook(webhook_id, webhook_token, wait \\ false) do
    request(:post, Constants.webhook_git(webhook_id, webhook_token), params: [wait: wait])
  end

  @doc """
  Deletes another user's reaction from a message

  Parameter `emoji` can be any of the following:

    * A `t:Nostrum.Struct.Emoji.emoji_api_name/0`.
    * A base 16 unicode emoji string.
    * A URI encoded string.

  ## Permissions

  This function requires that the nostrum user have the following permissions:

    * `MANAGE_MESSAGES`

  ## Examples

      iex> Nostrum.Api.delete_user_reaction(351194183568195585, 417954134373957633, "\xF0\x9F\x98\x81", 177888205536886784)
      {:ok}
      iex> Nostrum.Api.delete_user_reaction(351194183568195585, 417954134373957633, URI.encode("\u2b50"), 177888205536886784)
      {:ok}
  """
  @spec delete_user_reaction(Channel.id, Message.id, String.t | Emoji.emoji_api_name, User.id) :: error | {:ok}
  def delete_user_reaction(channel_id, message_id, emoji, user_id) do
    request(:delete, Constants.channel_reaction(channel_id, message_id, emoji, user_id))
  end

  @doc """
  Same as `delete_user_reaction/4`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_user_reaction!(Channel.id, Message.id, String.t | Emoji.emoji_api_name, User.id) :: no_return | {:ok}
  def delete_user_reaction!(channel_id, message_id, emoji, user_id) do
    delete_user_reaction(channel_id, message_id, emoji, user_id)
    |> bangify()
  end

  @doc """
  Gets a guild member by its `guild_id` and `user_id`.

  If the request was successful, this function returns `{:ok, member}`, where 
  `member` is a `Nostrum.Struct.Guild.Member`. Otherwise, this function 
  returns `{:error, reason}`.
  """
  @spec get_guild_member(Guild.id, User.id) :: error | {:ok, Member.t}
  def get_guild_member(guild_id, user_id) do
    request(:get, Constants.guild_member(guild_id, user_id))
    |> handle_request_with_decode({:struct, Member})
  end

  @doc """
  Same as `get_guild_member/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_member!(Guild.id, User.id) :: no_return | Member.t
  def get_guild_member!(guild_id, user_id) do
    get_guild_member(guild_id, user_id)
    |> bangify
  end

  @doc """
  Gets a list of members from a guild specified by `guild_id`.

  ## Request Params

  The following params are optional: 
 
    * `:limit` (integer) - max number of members to return (1-1000) (default: 1) 
    * `:after` (integer) - the highest user id in the previous page (default: 0)
 
  ## Examples
 
      iex> Nostrum.Api.list_guild_members(41771983423143937, limit: 1) 
      {:ok, [%Nostrum.Struct.Guild.Member{}]}
  """
  @spec list_guild_members(Guild.id, keyword | map) :: error | {:ok, [Member.t]}
  def list_guild_members(guild_id, params \\ [])
  def list_guild_members(guild_id, params) when is_list(params), 
    do: list_guild_members(guild_id, Map.new(params))
  
  def list_guild_members(guild_id, %{} = params) do
    request(:get, Constants.guild_members(guild_id), "", params: params)
    |> handle_request_with_decode({:list, {:struct, Member}})
  end

  @doc """
  Same as `list_guild_members/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec list_guild_members!(Guild.id, keyword | map) :: no_return | [Member.t]
  def list_guild_members!(guild_id, params \\ []) do
    list_guild_members(guild_id, params)
    |> bangify()
  end

  @doc """
  Adds a user to a guild. 
 
  The user's oauth2 access token is required for this function to work. 
 
  ## Request Params
 
  The following params are required: 
 
    * `:access_token` (string) - the user's oauth2 access token 
 
  The following params are optional: 
 
    * `:nick` (string) - value to set users nickname to 
    * `:roles` (list of `t:Nostrum.Struct.Guild.Role.id/0`) - array of role ids the member is assigned 
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened 
 
  ## Examples 
 
      iex> Nostrum.Api.add_guild_member(41771983423143937, 41771983423143937, access_token: "6qrZcUqja7812RVdnEKjpzOL4CvHBFG", nick: "nostrum", roles: [431849301, 431809431]) 
      {:ok, %Nostrum.Struct.Guild.Member{}}
  """
  @spec add_guild_member(Guild.id, User.id, keyword | map) :: error | {:ok, Member.t} 
  def add_guild_member(guild_id, user_id, params) 
  def add_guild_member(guild_id, user_id, params) when is_list(params), 
    do: add_guild_member(guild_id, user_id, Map.new(params))
  
  def add_guild_member(guild_id, user_id, %{} = params) do 
    request(:put, Constants.guild_member(guild_id, user_id), params)
    |> handle_request_with_decode({:struct, Member})
  end

  @doc """
  Same as `add_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec add_guild_member!(Guild.id, User.id, keyword | map) :: no_return | Member.t 
  def add_guild_member!(guild_id, user_id, params) do
    add_guild_member(guild_id, user_id, params)
    |> bangify()
  end

  @doc """
  Modify attributes of a guild member.
 
  ## Request Params
 
  The following params are optional:
 
    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Struct.Guild.Role.id/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened
    * `:channel_id` (`t:Nostrum.Struct.Guild.Channel.id/0`) - id of channel to move user to (if they are connected to voice)
 
  ## Examples
 
      iex> Nostrum.Api.modify_guild_member(41771983423143937, 41771983423143937, nick: "Nostrum")
      {:ok}
  """
  @spec modify_guild_member(Guild.id, User.id, keyword | map) :: error | {:ok}
  def modify_guild_member(guild_id, user_id, params \\ [])
  def modify_guild_member(guild_id, user_id, params) when is_list(params), 
    do: modify_guild_member(guild_id, user_id, Map.new(params))
  
  def modify_guild_member(guild_id, user_id, %{} = params) do
    request(:patch, Constants.guild_member(guild_id, user_id), params)
  end

  @doc """
  Same as `modify_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_member!(Guild.id, User.id, keyword | map) :: no_return | {:ok}
  def modify_guild_member!(guild_id, user_id, params \\ []) do
    modify_guild_member(guild_id, user_id, params)
    |> bangify()
  end

  @doc """
  Removes a member from a guild.

  ## Examples

      iex> Nostrum.Api.remove_guild_member(41771983423143937, 41771983423143937) 
      {:ok}
  """
  @spec remove_guild_member(Guild.id, User.id) :: error | {:ok}
  def remove_guild_member(guild_id, user_id) do
    request(:delete, Constants.guild_member(guild_id, user_id))
  end

  @doc """
  Same as `remove_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec remove_guild_member!(Guild.id, User.id) :: no_return | {:ok}
  def remove_guild_member!(guild_id, user_id) do
    remove_guild_member(guild_id, user_id)
    |> bangify()
  end

  @doc """
  Modify a guild channel's settings.
 
  Can modify the following types of channels: 
 
    * `0` - GUILD_TEXT
    * `2` - GUILD_VOICE
    * `4` - GUILD_CATEGORY
 
  ## Params
 
  All params are optional.
 
  The following params can be used with all guild channel types:
 
    * `:name` (string) - 2-100 character channel name
    * `:position` (integer) - the position of the channel in the left-hand listing
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0`) - channel or category-specific permissions
 
  The following params are limited to a specific type of channel:
 
    * `:nsfw` (boolean) (GUILD_TEXT only) - if the channel is nsfw 
    * `:topic` (string) (GUILD_TEXT only) - 0-1024 character channel topic 
    * `:bitrate` (integer) (GUILD_VOICE only) - the bitrate (in bits) of the voice channel; 8000 to 96000 (128000 for VIP servers)
    * `:user_limit` (integer) (GUILD_VOICE only) - the user limit of the voice channel; 0 refers to no limit, 1 to 99 refers to a user limit 
    * `:parent_id` (`t:Nostrum.Struct.Guild.Channel.id/0`) (GUILD_TEXT, GUILD_VOICE only) - id of the new parent category for a channel
 
  ## Examples
 
      Nostrum.Api.modify_channel(41771983423143933, name: "elixir-nostrum", topic: "nostrum discussion")

  """
  @spec modify_channel(Channel.id(), keyword | map) :: error | {:ok, Channel.t()}
  def modify_channel(channel_id, params)
  def modify_channel(channel_id, params) when is_list(params), 
    do: modify_channel(channel_id, Enum.into(params, %{}))

  def modify_channel(channel_id, %{} = params) do
    request(:patch, Constants.channel(channel_id), params)
    |> handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Same as `modify_channel/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_channel!(Channel.id(), keyword | map) :: no_return | Channel.t()
  def modify_channel!(channel_id, params) do
    modify_channel(channel_id, params)
    |> bangify()
  end

  @doc """ 
  Gets a list of guild channels from guild of id `guild_id`. 
 
  ## Examples
 
      Nostrum.Api.get_guild_channels(41771983423143933)

  """ 
  @spec get_guild_channels(Guild.id()) :: error | {:ok, [Channel.t()]} 
  def get_guild_channels(guild_id) do
    request(:get, Constants.guild_channels(guild_id))
    |> handle_request_with_decode({:list, {:struct, Channel}})
  end

  @doc """
  Same as `get_guild_channels/1`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_guild_channels!(Guild.id()) :: no_return | [Channel.t()]
  def get_guild_channels!(guild_id) do
    get_guild_channels(guild_id)
    |> bangify()
  end

  @doc """ 
  Creates a channel in a guild of id `guild_id`. 
 
  ## Request Params
 
  The following params are required: 
 
    * `:name` (string) - channel name (2-100 characters)
 
  The following params are optional: 
 
    * `:type` (integer) - the type of channel (See `Nostrum.Struct.Guild.Channel`) 
    * `:bitrate` (integer) - the bitrate (in bits) of the voice channel (voice only) 
    * `:user_limit` (integer) - the user limit of the voice channel (voice only) 
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0`) - the channel's permission overwrites 
    * `:parent_id` (`t:Nostrum.Struct.Guild.Channel.id/0`) - id of the parent category for a channel 
    * `:nsfw` (boolean) - if the channel is nsfw
 
  ## Examples
 
      Nostrum.Api.create_guild_channel(41771984817263543, name: "elixir-nostrum", type: 0, nsfw: false) 

  """ 
  @spec create_guild_channel(Guild.id(), keyword | map) :: error | {:ok, Channel.t()}
  def create_guild_channel(guild_id, params)
  def create_guild_channel(guild_id, params) when is_list(params), 
    do: create_guild_channel(guild_id, Map.new(params))
  
  def create_guild_channel(guild_id, %{} = params) do 
    request(:post, Constants.guild_channels(guild_id), params)
    |> handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Same as `create_guild_channel/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_channel!(Guild.id(), keyword | map) :: no_return | Channel.t()
  def create_guild_channel!(guild_id, params) do
    create_guild_channel(guild_id, params)
    |> bangify()
  end

  def get_application_information do
    request(:get, Constants.application_information)
    |> handle
  end

  @doc false
  def handle({:ok, body}), do: {:ok, Poison.decode!(body)}
  def handle(other), do: other

  def handle(payload, Guild) do
    with {:ok, body} <- payload do
      map = Poison.decode!(body)
      atom_map = Util.safe_atom_map(map)
      {:ok, GuildServer.index_guild(atom_map)}
    end
  end

  def handle(payload, [as]) do
    with {:ok, body} <- payload,
    do: {:ok, Poison.decode!(body, as: [apply(as, :p_encode, [])])}
  end

  def handle(payload, as) do
    with {:ok, body} <- payload,
    do: {:ok, Poison.decode!(body, as: apply(as, :p_encode, []))}
  end

  # HTTPosion defaults to `""` for an empty body, so it's safe to do so here
  def request(method, route, body \\ "", options \\ []) do
    request = %{
      method: method,
      route: route,
      body: body,
      options: options,
      headers: [{"content-type", "application/json"}]
    }
    GenServer.call(Ratelimiter, {:queue, request, nil}, :infinity)
  end

  def request_multipart(method, route, body \\ "", options \\ []) do
    request = %{
      method: method,
      route: route,
      # Hello hackney documentation :^)
      body: {:multipart, [
        {
          :file,
          body.file,
          {"form-data", [{"filename", body.content}]},
          [{"tts", body.tts}]
        }
      ]},
      options: options,
      headers: [{"content-type", "multipart/form-data"}]
    }
    GenServer.call(Ratelimiter, {:queue, request, nil}, :infinity)
  end

  @doc false
  def bangify(to_bang) do
    case to_bang do
      {:error, %{status_code: code, message: message}} ->
        raise(Nostrum.Error.ApiError, status_code: code, message: message)
      {:ok, body} ->
        body
      {:ok} ->
        {:ok}
    end
  end

  @doc """
  Returns the token of the bot.
  """
  @spec get_token() :: String.t
  def get_token do
    Application.get_env(:nostrum, :token)
  end

  defp do_create_message(channel_id, params)
  defp do_create_message(channel_id, params) when is_list(params), do: do_create_message(channel_id, Map.new(params))
  
  defp do_create_message(channel_id, %{file: file_path} = params) do
    payload_json = 
      params
      |> Map.delete(:file)
      |> Poison.encode!()

    multipart = [
      {:file, file_path},
      {"payload_json", payload_json}
    ]

    request = %{
      method: :post,
      route: Constants.channel_messages(channel_id),
      body: {:multipart, multipart},
      options: [],
      headers: [
        {"content-type", "multipart/form-data"}
      ]
    }

    GenServer.call(Ratelimiter, {:queue, request, nil}, :infinity)
    |> handle_request_with_decode({:struct, Message})
  end

  defp do_create_message(channel_id, %{} = params) do
    request(:post, Constants.channel_messages(channel_id), params)
    |> handle_request_with_decode({:struct, Message})
  end

  defp handle_request_with_decode(response, type)
  defp handle_request_with_decode({:error, _} = error, _type), do: error

  defp handle_request_with_decode({:ok, body}, type) do
    convert = 
      body
      |> Poison.decode!()
      |> Util.cast(type)

    {:ok, convert}
  end  
end
