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

  import Nostrum.Struct.Snowflake, only: [is_snowflake: 1]

  alias Nostrum.{Constants, Util}
  alias Nostrum.Cache.Guild.GuildServer
  alias Nostrum.Struct.{Embed, Guild, Message, User, Webhook}
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild.{Member, Channel, Role}
  alias Nostrum.Shard.{Supervisor, Session}

  @typedoc """
  Represents a failed response from the API.

  This occurs when hackney or HTTPoison fail, or when the API doesn't respond with `200` or `204`.
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
  @type emoji :: Emoji.t() | Emoji.emoji_api_name()

  @typedoc """
  Represents optional parameters for Api functions.

  Each function has documentation regarding what parameters it
  supports or needs.
  """
  @type options :: keyword | map

  @doc """
  Updates the status of the bot for a certain shard.

  ## Parameters
    - `pid` - Pid of the shard.
    - `status` - Status of the bot.
    - `game` - The 'playing' text of the bot. Empty will clear.
    - `type` - The type of status to show. 0 (Playing) | 1 (Streaming) | 2 (Listening) | 3 (Watching)
    - `stream` - URL of twitch.tv stream
  """
  @spec update_shard_status(pid, status, String.t(), integer, String.t()) :: :ok
  def update_shard_status(pid, status, game, type \\ 0, stream \\ nil) do
    Session.update_status(pid, to_string(status), game, stream, type)
    :ok
  end

  @doc """
  Updates the status of the bot for all shards.

  See `update_shard_status/4` for usage.
  """
  @spec update_status(status, String.t(), integer, String.t()) :: :ok
  def update_status(status, game, type \\ 0, stream \\ nil) do
    Supervisor.update_status(status, game, stream, type)
    :ok
  end

  @doc ~S"""
  Posts a message to a guild text or DM channel.

  This endpoint requires the `VIEW_CHANNEL` and `SEND_MESSAGES` permissions. It
  may situationally need the `SEND_MESSAGES_TTS` permission. It fires the
  `t:Nostrum.Consumer.message_create/0` event.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:content` (string) - the message contents (up to 2000 characters)
    * `:nonce` (`t:Nostrum.Struct.Snowflake.t/0`) - a nonce that can be used for
    optimistic message sending
    * `:tts` (boolean) - true if this is a TTS message
    * `:file` (`t:Path.t/0`) - the path of the file being sent
    * `:embed` (`t:Nostrum.Struct.Embed.t/0`) - embedded rich content

    At least one of the following is required: `:content`, `:file`, `:embed`.

  ## Examples

  ```Elixir
  Nostrum.Api.create_message(43189401384091, content: "hello world!")

  import Nostrum.Struct.Embed
  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("embed")
    |> put_description("new desc")
  Nostrum.Api.create_message(43189401384091, embed: embed)

  Nostrum.Api.create_message(43189401384091, file: "/path/to/file.txt")

  Nostrum.Api.create_message(43189401384091, content: "hello world!", embed: embed, file: "/path/to/file.txt")
  ```
  """
  @spec create_message(Channel.id() | Message.t(), options) :: error | {:ok, Message.t()}
  def create_message(channel_id, options)

  def create_message(%Message{} = message, options),
    do: create_message(message.channel_id, options)

  def create_message(channel_id, options) when is_list(options),
    do: create_message(channel_id, Map.new(options))

  def create_message(channel_id, %{} = options) when is_snowflake(channel_id) do
    case options do
      %{file: _} -> create_message_with_multipart(channel_id, options)
      _ -> create_message_with_json(channel_id, options)
    end
  end

  defp create_message_with_multipart(channel_id, %{file: file_path} = options) do
    payload_json =
      options
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

  defp create_message_with_json(channel_id, options) do
    request(:post, Constants.channel_messages(channel_id), options)
    |> handle_request_with_decode({:struct, Message})
  end

  @doc ~S"""
  Same as `create_message/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_message!(Channel.id() | Message.t(), options) :: no_return | Message.t()
  def create_message!(channel_id, options) do
    create_message(channel_id, options)
    |> bangify
  end

  @doc ~S"""
  Edits a previously sent message in a channel.

  This endpoint requires the `VIEW_CHANNEL` permission. It fires the
  `t:Nostrum.Consumer.message_update/0` event.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:content` (string) - the message contents (up to 2000 characters)
    * `:embed` (`t:Nostrum.Struct.Embed.t/0`) - embedded rich content

  ## Examples

  ```Elixir
  Nostrum.Api.edit_message(43189401384091, 1894013840914098, content: "hello world!")

  import Nostrum.Struct.Embed
  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("embed")
    |> put_description("new desc")
  Nostrum.Api.edit_message(43189401384091, 1894013840914098, embed: embed)

  Nostrum.Api.edit_message(43189401384091, 1894013840914098, content: "hello world!", embed: embed)
  ```
  """
  @spec edit_message(Channel.id(), Message.id(), options) :: error | {:ok, Message.t()}
  def edit_message(channel_id, message_id, options)

  def edit_message(channel_id, message_id, options) when is_list(options),
    do: edit_message(channel_id, message_id, Map.new(options))

  def edit_message(channel_id, message_id, %{} = options)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    request(:patch, Constants.channel_message(channel_id, message_id), options)
    |> handle_request_with_decode({:struct, Message})
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
  Same as `delete_message/2`, but takes a `Nostrum.Struct.Message`.
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

  ```Elixir
  Nostrum.Api.delete_message(43189401384091, 43189401384091)
  ```
  """
  @spec delete_message(Channel.id(), Message.id()) :: error | {:ok}
  def delete_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    request(:delete, Constants.channel_message(channel_id, message_id))
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

  ```Elixir
  # Using a Nostrum.Struct.Emoji.
  emoji = %Nostrum.Struct.Emoji{id: 43819043108, name: "foxbot"}
  Nostrum.Api.create_reaction(123123123123, 321321321321, emoji)

  # Using a base 16 emoji string.
  Nostrum.Api.create_reaction(123123123123, 321321321321, "\xF0\x9F\x98\x81")

  ```

  For other emoji string examples, see `t:Nostrum.Struct.Emoji.emoji_api_name/0`.
  """
  @spec create_reaction(Channel.id(), Message.id(), emoji) :: error | {:ok}
  def create_reaction(channel_id, message_id, emoji)

  def create_reaction(channel_id, message_id, %Emoji{} = emoji),
    do: create_reaction(channel_id, message_id, Emoji.get_api_name(emoji))

  def create_reaction(channel_id, message_id, emoji_api_name) do
    request(:put, Constants.channel_reaction_me(channel_id, message_id, emoji_api_name))
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
    do: delete_own_reaction(channel_id, message_id, Emoji.get_api_name(emoji))

  def delete_own_reaction(channel_id, message_id, emoji_api_name) do
    request(:delete, Constants.channel_reaction_me(channel_id, message_id, emoji_api_name))
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
    do: delete_user_reaction(channel_id, message_id, Emoji.get_api_name(emoji), user_id)

  def delete_user_reaction(channel_id, message_id, emoji_api_name, user_id) do
    request(:delete, Constants.channel_reaction(channel_id, message_id, emoji_api_name, user_id))
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
  Gets all users who reacted with an emoji.

  This endpoint requires the `VIEW_CHANNEL` and `READ_MESSAGE_HISTORY` permissions.

  If successful, returns `{:ok, users}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `create_reaction/3` for similar examples.
  """
  @spec get_reactions(Channel.id(), Message.id(), emoji) :: error | {:ok, [User.t()]}
  def get_reactions(channel_id, message_id, emoji)

  def get_reactions(channel_id, message_id, %Emoji{} = emoji),
    do: get_reactions(channel_id, message_id, Emoji.get_api_name(emoji))

  def get_reactions(channel_id, message_id, emoji_api_name) do
    request(:get, Constants.channel_reactions_get(channel_id, message_id, emoji_api_name))
    |> handle_request_with_decode({:list, {:struct, User}})
  end

  @doc ~S"""
  Same as `get_reactions/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_reactions!(Channel.id(), Message.id(), emoji) :: no_return | [User.t()]
  def get_reactions!(channel_id, message_id, emoji) do
    get_reactions(channel_id, message_id, emoji)
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
    request(:delete, Constants.channel_reactions_delete(channel_id, message_id))
  end

  @doc ~S"""
  Same as `delete_all_reactions/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_all_reactions!(Channel.id(), Message.id()) :: no_return | {:ok}
  def delete_all_reactions!(channel_id, message_id) do
    delete_all_reactions(channel_id, message_id)
    |> bangify
  end

  @doc """
  Get a channel.

  Gets a channel specified by `id`.
  """
  @spec get_channel(integer) :: error | {:ok, Nostrum.Struct.Channel.t()}
  def get_channel(channel_id) do
    case request(:get, Constants.channel(channel_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Get a channel.

  Gets a channel specified by `id`.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec get_channel!(integer) :: no_return | Nostrum.Struct.Channel.t()
  def get_channel!(channel_id) do
    get_channel(channel_id)
    |> bangify
  end

  @doc """
  Edit a channel.

  Edits a channel with `options`

  `options` is a kwl with the following optional keys:
   * `name` - New name of the channel.
   * `position` - Position of the channel.
   * `topic` - Topic of the channel. *Text Channels only*
   * `bitrate` - Bitrate of the voice channel. *Voice Channels only*
   * `user_limit` - User limit of the channel. 0 for no limit. *Voice Channels only*
  """
  @spec edit_channel(
          integer,
          name: String.t(),
          position: integer,
          topic: String.t(),
          bitrate: String.t(),
          user_limit: integer
        ) :: error | {:ok, Nostrum.Struct.Channel.t()}
  def edit_channel(channel_id, options) do
    case request(:patch, Constants.channel(channel_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Edit a channel.

  See `edit_channel/2` for parameters.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec edit_channel!(
          integer,
          name: String.t(),
          position: integer,
          topic: String.t(),
          bitrate: String.t(),
          user_limit: integer
        ) :: error | {:ok, Nostrum.Struct.Channel.t()}
  def edit_channel!(channel_id, options) do
    edit_channel(channel_id, options)
    |> bangify
  end

  @doc """
  Delete a channel.

  Channel to delete is specified by `channel_id`.
  """
  @spec delete_channel(integer) :: error | {:ok, Nostrum.Struct.Channel.t()}
  def delete_channel(channel_id) do
    case request(:delete, Constants.channel(channel_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Delete a channel.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec delete_channel!(integer) :: no_return | Nostrum.Struct.Channel.t()
  def delete_channel!(channel_id) do
    delete_channel(channel_id)
    |> bangify
  end

  @doc ~S"""
  Retrives a channel's messages around a `locator` up to a `limit`.

  This endpoint requires the 'VIEW_CHANNEL' permission. If the current user
  is missing the 'READ_MESSAGE_HISTORY' permission, then this function will
  return no messages.

  If successful, returns `{:ok, messages}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```Elixir
  Nostrum.Api.get_channel_messages(43189401384091, 5, {:before 130230401384})
  ```
  """
  @spec get_channel_messages(Channel.id(), limit, locator) :: error | {:ok, [Message.t()]}
  def get_channel_messages(channel_id, limit, locator \\ {}) when is_snowflake(channel_id) do
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
      {:error, message} ->
        {:error, message}

      {:ok, []} ->
        {:ok, messages}

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

  @doc ~S"""
  Same as `get_channel_messages/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec get_channel_messages!(Channel.id(), limit, locator) :: no_return | [Message.t()]
  def get_channel_messages!(channel_id, limit, locator) do
    get_channel_messages(channel_id, limit, locator)
    |> bangify
  end

  @doc ~S"""
  Retrieves a message from a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'READ_MESSAGE_HISTORY' permissions.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```Elixir
  Nostrum.Api.get_channel_message(43189401384091, 198238475613443)
  ```
  """
  @spec get_channel_message(Channel.id(), Message.id()) :: error | {:ok, Message.t()}
  def get_channel_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    request(:get, Constants.channel_message(channel_id, message_id))
    |> handle_request_with_decode({:struct, Message})
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

  This method can only delete messages sent within the last two weeks.
  `Filter` is an optional parameter that specifies whether messages sent over
  two weeks ago should be filtered out; defaults to `true`.
  """
  @spec bulk_delete_messages(integer, [Nostrum.Struct.Message.id()], boolean) :: error | {:ok}
  def bulk_delete_messages(channel_id, messages, filter \\ true)

  def bulk_delete_messages(channel_id, messages, false),
    do: request(:post, Constants.channel_bulk_delete(channel_id), %{messages: messages})

  def bulk_delete_messages(channel_id, messages, true) do
    filter_before = ((Util.now() - 14 * 24 * 60 * 60) * 1000 - 1_420_070_400_000) <<< 22

    filtered_messages =
      Enum.filter(messages, fn message_id ->
        message_id > filter_before
      end)

    request(:post, Constants.channel_bulk_delete(channel_id), %{messages: filtered_messages})
  end

  @doc """
  Deletes multiple messages from a channel.

  See `bulk_delete_messages/2` for more info.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec bulk_delete_messages!(integer, [Nostrum.Struct.Message.id()], boolean) ::
          no_return | {:ok}
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
  @spec edit_channel_permissions(
          integer,
          integer,
          allow: integer,
          deny: integer,
          type: String.t()
        ) :: error | {:ok}
  def edit_channel_permissions(channel_id, overwrite_id, permission_info) do
    request(:put, Constants.channel_permission(channel_id, overwrite_id), permission_info)
  end

  @doc """
  Edit the permission overwrites for a user or role.

  See `edit_channel_permissions/2` for more info.

  Raises `Nostrum.Error.ApiError` if error occurs while making the rest call.
  """
  @spec edit_channel_permissions!(
          integer,
          integer,
          allow: integer,
          deny: integer,
          type: String.t()
        ) :: no_return | {:ok}
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
  @spec get_channel_invites(integer) :: error | {:ok, [Nostrum.Struct.Invite.t()]}
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
  @spec create_channel_invite(
          integer,
          max_age: integer,
          max_uses: integer,
          temporary: boolean,
          unique: boolean
        ) :: error | {:ok, Nostrum.Struct.Invite.t()}
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

  @doc ~S"""
  Retrieves all pinned messages from a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'READ_MESSAGE_HISTORY' permissions.

  If successful, returns `{:ok, messages}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```Elixir
  Nostrum.Api.get_pinned_messages(43189401384091)
  ```
  """
  @spec get_pinned_messages(Channel.id()) :: error | {:ok, [Message.t()]}
  def get_pinned_messages(channel_id) when is_snowflake(channel_id) do
    request(:get, Constants.channel_pins(channel_id))
    |> handle_request_with_decode({:list, {:struct, Message}})
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

  ```Elixir
  Nostrum.Api.add_pinned_channel_message(43189401384091, 18743893102394)
  ```
  """
  @spec add_pinned_channel_message(Channel.id(), Message.id()) :: error | {:ok}
  def add_pinned_channel_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    request(:put, Constants.channel_pin(channel_id, message_id))
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
    request(:delete, Constants.channel_pin(channel_id, message_id))
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
    request(:get, Constants.guild_emojis(guild_id))
    |> handle_request_with_decode({:list, {:struct, Emoji}})
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
    request(:get, Constants.guild_emoji(guild_id, emoji_id))
    |> handle_request_with_decode({:struct, Emoji})
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

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the emoji
    * `:image` (base64 data URI) - the 128x128 emoji image. Maximum size of 256kb
    * `:roles` (list of `t:Nostrum.Struct.Snowflake.t/0`) - roles for which this emoji will be whitelisted
    (default: [])

  `:name` and `:image` are always required.

  ## Examples

  ```Elixir
  image = "data:image/png;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8="

  Nostrum.Api.create_guild_emoji(43189401384091, name: "nostrum", image: image, roles: [])
  ```
  """
  @spec create_guild_emoji(Guild.id(), options) :: error | {:ok, Emoji.t()}
  def create_guild_emoji(guild_id, options)

  def create_guild_emoji(guild_id, options) when is_list(options),
    do: create_guild_emoji(guild_id, Map.new(options))

  def create_guild_emoji(guild_id, %{} = options) do
    request(:post, Constants.guild_emojis(guild_id), options)
    |> handle_request_with_decode({:struct, Emoji})
  end

  @doc ~S"""
  Same as `create_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_emoji!(Guild.id(), options) :: no_return | Emoji.t()
  def create_guild_emoji!(guild_id, params) do
    create_guild_emoji(guild_id, params)
    |> bangify
  end

  @doc ~S"""
  Modify the given emoji.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the emoji
    * `:roles` (list of `t:Nostrum.Struct.Snowflake.t/0`) - roles to which this emoji will be whitelisted

  ## Examples

  ```Elixir
  Nostrum.Api.modify_guild_emoji(43189401384091, 4314301984301, name: "elixir", roles: [])
  ```
  """
  @spec modify_guild_emoji(Guild.id(), Emoji.id(), options) :: error | {:ok, Emoji.t()}
  def modify_guild_emoji(guild_id, emoji_id, options \\ %{})

  def modify_guild_emoji(guild_id, emoji_id, options) when is_list(options),
    do: modify_guild_emoji(guild_id, emoji_id, Map.new(options))

  def modify_guild_emoji(guild_id, emoji_id, %{} = options) do
    request(:patch, Constants.guild_emoji(guild_id, emoji_id), options)
    |> handle_request_with_decode({:struct, Emoji})
  end

  @doc ~S"""
  Same as `modify_guild_emoji/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_emoji!(Guild.id(), Emoji.id(), options) :: no_return | Emoji.t()
  def modify_guild_emoji!(guild_id, emoji_id, options) do
    modify_guild_emoji(guild_id, emoji_id, options)
    |> bangify
  end

  @doc ~S"""
  Deletes the given emoji.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec delete_guild_emoji(Guild.id(), Emoji.id()) :: error | {:ok}
  def delete_guild_emoji(guild_id, emoji_id),
    do: request(:delete, Constants.guild_emoji(guild_id, emoji_id))

  @doc ~S"""
  Same as `delete_guild_emoji/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec delete_guild_emoji!(Guild.id(), Emoji.id()) :: no_return | {:ok}
  def delete_guild_emoji!(guild_id, emoji_id) do
    delete_guild_emoji(guild_id, emoji_id)
    |> bangify
  end

  @doc """
  Gets a guild using the REST api

  Retrieves a guild with specified `guild_id`.

  Returns {:ok, Nostrum.Struct.Guild.t} if successful, `error` otherwise.
  """
  @spec get_guild(integer) :: error | {:ok, Nostrum.Struct.Guild.t()}
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
  @spec get_guild!(integer) :: no_return | Nostrum.Struct.Guild.t()
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
          name: String.t(),
          region: String.t(),
          verification_level: integer,
          default_message_notifications: boolean,
          afk_channel_id: integer,
          afk_timeout: integer,
          icon: String.t(),
          owner_id: integer,
          splash: String.t()
        }) :: error | {:ok, Nostrum.Struct.Guild.t()}
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
  Gets a list of channels.

  Guild to get channels for is specified by `guild_id`.
  """
  @spec get_channels(integer) :: error | {:ok, Nostrum.Struct.Channel.t()}
  def get_channels(guild_id) do
    case request(:get, Constants.guild_channels(guild_id)) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Creates a channel.

  Guild to create channel in is specifed by `guild_id`.

  `options` is a map with the following optional keys (except for name):
   * `name` - Channel name.
   * `type` - Channel type.
   * `bitrate` - Bitrate if creating voice channel.
   * `user_limit` - User limit if creating voice channel.
   * `permission_overwrites` - Array of permission overwrites.
  """
  @spec create_channel(integer, %{
          name: String.t(),
          type: String.t(),
          bitrate: integer,
          user_limit: integer,
          permission_overwrites: [Nostrum.Struct.Overwrite.t()]
        }) :: error | {:ok, Nostrum.Struct.Channel.t()}
  def create_channel(guild_id, options) do
    case request(:post, Constants.guild_channels(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Reorders a guild's channels.

  Guild to modify channels for is specified by `guild_id`.

  `options` is a list of maps with the following keys:
   * `id` - Id of the channel.
   * `position` - Sorting position of the channel.
  """
  @spec modify_channel_position(integer, [
          %{
            id: integer,
            position: integer
          }
        ]) :: error | {:ok, [Nostrum.Struct.Guild.Channel.t()]}
  def modify_channel_position(guild_id, options) do
    case request(:patch, Constants.guild_channels(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Gets a guild member.

  If successful, returns `{:ok, member}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```Elixir
  Nostrum.Api.get_guild_member(4019283754613, 184937267485)
  ```
  """
  @spec get_guild_member(Guild.id(), User.id()) :: error | {:ok, Member.t()}
  def get_guild_member(guild_id, user_id) when is_snowflake(guild_id) and is_snowflake(user_id) do
    request(:get, Constants.guild_member(guild_id, user_id))
    |> handle_request_with_decode({:struct, Member})
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

  ```Elixir
  Nostrum.Api.list_guild_members(41771983423143937, limit: 1)
  ```
  """
  @spec list_guild_members(Guild.id(), options) :: error | {:ok, [Member.t()]}
  def list_guild_members(guild_id, options \\ %{})

  def list_guild_members(guild_id, options) when is_list(options),
    do: list_guild_members(guild_id, Map.new(options))

  def list_guild_members(guild_id, %{} = options) when is_snowflake(guild_id) do
    request(:get, Constants.guild_members(guild_id), "", params: options)
    |> handle_request_with_decode({:list, {:struct, Member}})
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

  If successful, returns `{:ok, member}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:access_token` (string) - the user's oauth2 access token
    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Struct.Guild.Role.id/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened

  `:access_token` is always required.

  ## Examples

  ```Elixir
  Nostrum.Api.add_guild_member(
    41771983423143937,
    18374719829378473,
    access_token: "6qrZcUqja7812RVdnEKjpzOL4CvHBFG",
    nick: "nostrum",
    roles: [431849301, 913809431]
  )
  ```
  ```
  """
  @spec add_guild_member(Guild.id(), User.id(), options) :: error | {:ok, Member.t()}
  def add_guild_member(guild_id, user_id, options)

  def add_guild_member(guild_id, user_id, options) when is_list(options),
    do: add_guild_member(guild_id, user_id, Map.new(options))

  def add_guild_member(guild_id, user_id, %{} = options)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    request(:put, Constants.guild_member(guild_id, user_id), options)
    |> handle_request_with_decode({:struct, Member})
  end

  @doc """
  Same as `add_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec add_guild_member!(Guild.id(), User.id(), options) :: no_return | Member.t()
  def add_guild_member!(guild_id, user_id, options) do
    add_guild_member(guild_id, user_id, options)
    |> bangify
  end

  @doc ~S"""
  Modifies a guild member's attributes.

  This endpoint fires the `t:Nostrum.Consumer.guild_member_update/0` event.
  It situationally requires the `MANAGE_NICKNAMES`, `MANAGE_ROLES`,
  `MUTE_MEMBERS`, `DEAFEN_MEMBERS`, and `MOVE_MEMBERS` permissions.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Struct.Snowflake.t/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened
    * `:channel_id` (`t:Nostrum.Struct.Snowflake.t/0`) - id of channel to move user to (if they are connected to voice)

  ## Examples

  ```Elixir
  Nostrum.Api.modify_guild_member(41771983423143937, 637162356451, nick: "Nostrum")
  {:ok}
  ```
  """
  @spec modify_guild_member(Guild.id(), User.id(), options) :: error | {:ok}
  def modify_guild_member(guild_id, user_id, options \\ %{})

  def modify_guild_member(guild_id, user_id, options) when is_list(options),
    do: modify_guild_member(guild_id, user_id, Map.new(options))

  def modify_guild_member(guild_id, user_id, %{} = options)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    request(:patch, Constants.guild_member(guild_id, user_id), options)
  end

  @doc """
  Same as `modify_guild_member/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_guild_member!(Guild.id(), User.id(), options) :: error | {:ok}
  def modify_guild_member!(guild_id, user_id, options \\ %{}) do
    modify_guild_member(guild_id, user_id, options)
    |> bangify
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

  @doc """
  Removes a member from a guild.

  This event requires the `KICK_MEMBERS` permission. It fires a
  `t:Nostrum.Consumer.guild_member_remove/0` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```Elixir
  Nostrum.Api.remove_guild_member(1453827904102291, 18739485766253)
  {:ok}
  ```
  """
  @spec remove_guild_member(Guild.id(), User.id()) :: error | {:ok}
  def remove_guild_member(guild_id, user_id)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    request(:delete, Constants.guild_member(guild_id, user_id))
  end

  @doc """
  Same as `remove_guild_member/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec remove_guild_member!(Guild.id(), User.id()) :: no_return | {:ok}
  def remove_guild_member!(guild_id, user_id) do
    remove_guild_member(guild_id, user_id)
    |> bangify
  end

  @doc """
  Gets a list of users banend from a guild.

  Guild to get bans for is specified by `guild_id`.
  """
  @spec get_guild_bans(integer) :: error | {:ok, [Nostrum.Struct.User.t()]}
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

  @doc ~S"""
  Gets a guild's roles.

  If successful, returns `{:ok, roles}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```Elixir
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

  @doc """
  Creates a guild role.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_create/0` event.

  If successful, returns `{:ok, role}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the role (default: "new role")
    * `:permissions` (integer) - bitwise of the enabled/disabled permissions (default: @everyone perms)
    * `:color` (integer) - RGB color value (default: 0)
    * `:hoist` (boolean) - whether the role should be displayed separately in the sidebar (default: false)
    * `:mentionable` (boolean) - whether the role should be mentionable (default: false)

  ## Examples

  ```Elixir
  Nostrum.Api.create_guild_role(41771983423143937, name: "nostrum-club", hoist: true)
  ```
  """
  @spec create_guild_role(Guild.id(), options) :: error | {:ok, Role.t()}
  def create_guild_role(guild_id, options)

  def create_guild_role(guild_id, options) when is_list(options),
    do: create_guild_role(guild_id, Map.new(options))

  def create_guild_role(guild_id, %{} = options) when is_snowflake(guild_id) do
    request(:post, Constants.guild_roles(guild_id), options)
    |> handle_request_with_decode({:struct, Role})
  end

  @doc ~S"""
  Same as `create_guild_role/2`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec create_guild_role!(Guild.id(), options) :: no_return | Role.t()
  def create_guild_role!(guild_id, options) do
    create_guild_role(guild_id, options)
    |> bangify
  end

  @doc """
  Reorders a guild's roles.

  Guild to modify roles for is specified by `guild_id`.

  `options` is a list of maps with the following keys:
   * `id` - Id of the role.
   * `position` - Sorting position of the role.
  """
  @spec modify_guild_role_positions(integer, [
          %{
            id: integer,
            position: integer
          }
        ]) :: error | {:ok, [Nostrum.Struct.Guild.Role.t()]}
  def modify_guild_role_positions(guild_id, options) do
    case request(:patch, Constants.guild_roles(guild_id), options) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Modifies a guild role.

  ## Parameter
  `guild_id` - Guild to modify role for.
  `role_id` - Role to modify.
  `options` - Map with the following *optional* keys:
    - `name` - Name of the role.
    - `permissions` - Bitwise of the enabled/disabled permissions.
    - `color` - RGB color value.
    - `hoist` - Whether the role should be displayed seperately in the sidebar.
    - `mentionable` - Whether the role should be mentionable.
  """
  @spec modify_guild_role(Guild.id(), Role.id(), %{
          name: String.t(),
          permissions: integer,
          color: integer,
          hoist: boolean,
          mentionable: boolean
        }) :: error | {:ok, Nostrum.Struct.Guild.Role.t()}
  def modify_guild_role(guild_id, role_id, options) do
    request(:patch, Constants.guild_role(guild_id, role_id), options)
    |> handle(Role)
  end

  @doc """
  Deletes a guild role.

  Role to delte is specified by `guild_id` and `role_id`
  """
  @spec delete_guild_role(integer, integer) :: error | {:ok}
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
  @spec get_voice_region(integer) :: error | {:ok, [Nostrum.Struct.VoiceRegion.t()]}
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
  @spec get_guild_invites(integer) :: error | {:ok, [Nostrum.Struct.Invite.t()]}
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
  @spec get_guild_integrations(integer) :: error | {:ok, [Nostrum.Struct.Guild.Integration.t()]}
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
          type: String.t(),
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
  @spec get_invite(integer) :: error | {:ok, Nostrum.Struct.Invite.t()}
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
  @spec delete_invite(integer) :: error | {:ok, Nostrum.Struct.Invite.t()}
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
  @spec accept_invite(integer) :: error | {:ok, Nostrum.Struct.Invite.t()}
  def accept_invite(invite_code) do
    request(:post, Constants.invite(invite_code))
  end

  @doc """
  Gets a user by its `t:Nostrum.Struct.User.id/0`.

  If the request is successful, this function returns `{:ok, user}`, where
  `user` is a `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get_user(User.id()) :: error | {:ok, User.t()}
  def get_user(user_id) do
    request(:get, Constants.user(user_id))
    |> handle_request_with_decode({:struct, User})
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

  If nostrum's caching is enabled, it is recommended to use `Nostrum.Cache.Me.get/0`
  instead of this function. This is because sending out an API request is much slower
  than pulling from our cache.

  If the request is successful, this function returns `{:ok, user}`, where
  `user` is nostrum's `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get_current_user() :: error | {:ok, User.t()}
  def get_current_user do
    request(:get, Constants.me())
    |> handle_request_with_decode({:struct, User})
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
    * `:avatar` (string) - the user's avatar as [avatar data](https://discordapp.com/developers/docs/resources/user#avatar-data)

  ## Examples

  ```Elixir
  Nostrum.Api.modify_current_user(avatar: "data:image/jpeg;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8=")
  ```
  """
  @spec modify_current_user(keyword | map) :: error | {:ok, User.t()}
  def modify_current_user(options)

  def modify_current_user(options) when is_list(options),
    do: modify_current_user(Map.new(options))

  def modify_current_user(%{} = options) do
    request(:patch, Constants.me(), options)
    |> handle_request_with_decode({:struct, User})
  end

  @doc """
  Same as `modify_current_user/3`, but raises `Nostrum.Error.ApiError` in case of failure.
  """
  @spec modify_current_user!(keyword | map) :: no_return | User.t()
  def modify_current_user!(options) do
    modify_current_user(options)
    |> bangify
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
        }) :: error | {:ok, [Nostrum.Struct.Guild.t()]}
  def get_current_users_guilds(options \\ %{}) do
    case request(:get, Constants.me_guilds(), "", params: options) do
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
  @spec get_user_dms() :: error | {:ok, [Nostrum.Struct.DMChannel.t()]}
  def get_user_dms do
    case request(:get, Constants.me_channels()) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Creates a new DM channel.

  Opens a DM channel with the user specified by `user_id`.
  """
  @spec create_dm(integer) :: error | {:ok, Nostrum.Struct.DMChannel.t()}
  def create_dm(user_id) do
    case request(:post, Constants.me_channels(), %{recipient_id: user_id}) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Creates a new group DM channel.
  """
  @spec create_group_dm([String.t()], map) :: error | {:ok, Nostrum.Struct.DMChannel.t()}
  def create_group_dm(access_tokens, nicks) do
    case request(:post, Constants.me_channels(), %{access_tokens: access_tokens, nicks: nicks}) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Gets a list of user connections.
  """
  @spec get_user_connections() :: error | {:ok, Nostrum.Struct.User.Connection.t()}
  def get_user_connections do
    case request(:get, Constants.me_connections()) do
      {:ok, body} ->
        {:ok, Poison.decode!(body)}

      other ->
        other
    end
  end

  @doc """
  Gets a list of voice regions.
  """
  @spec list_voice_regions() :: error | {:ok, [Nostrum.Struct.VoiceRegion.t()]}
  def list_voice_regions do
    case request(:get, Constants.regions()) do
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
  @spec create_webhook(Channel.id(), %{
          name: String.t(),
          avatar: String.t()
        }) :: error | {:ok, Nostrum.Struct.Webhook.t()}
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
  @spec get_channel_webhooks(Channel.id()) :: error | {:ok, [Nostrum.Struct.Webhook.t()]}
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
  @spec get_guild_webhooks(Guild.id()) :: error | {:ok, [Nostrum.Struct.Webhook.t()]}
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
  @spec get_webhook(Webhook.id()) :: error | {:ok, Nostrum.Struct.Webhook.t()}
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
  @spec get_webhook_with_token(Webhook.id(), Webhook.token()) ::
          error | {:ok, Nostrum.Struct.Webhook.t()}
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
  @spec modify_webhook(Webhook.id(), %{
          name: String.t(),
          avatar: String.t()
        }) :: error | {:ok, Nostrum.Struct.Webhook.t()}
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
  @spec modify_webhook_with_token(Webhook.id(), Webhook.token(), %{
          name: String.t(),
          avatar: String.t()
        }) :: error | {:ok, Nostrum.Struct.Webhook.t()}
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
  @spec delete_webhook(Webhook.id()) :: error | {:ok}
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
  @spec execute_webhook(
          Webhook.id(),
          Webhook.token(),
          %{
            content: String.t(),
            username: String.t(),
            avatar_url: String.t(),
            tts: boolean,
            file: String.t(),
            embeds: Embed.t()
          },
          boolean
        ) :: error | {:ok}
  def execute_webhook(webhook_id, webhook_token, args, wait \\ false)

  def execute_webhook(webhook_id, webhook_token, %{file: _} = args, wait) do
    case request_multipart(
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
    case request(
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
  @spec execute_slack_webhook(Webhook.id(), Webhook.token(), boolean) :: error | {:ok}
  def execute_slack_webhook(webhook_id, webhook_token, wait \\ false) do
    request(:post, Constants.webhook_slack(webhook_id, webhook_token), params: [wait: wait])
  end

  @doc """
  Executes a git webhook.

  ## Parameters
    - `webhook_id` - Id of webhook to delete.
    - `webhook_token` - Token of the webhook to delete.
  """
  @spec execute_git_webhook(Webhook.id(), Webhook.token(), boolean) :: error | {:ok}
  def execute_git_webhook(webhook_id, webhook_token, wait \\ false) do
    request(:post, Constants.webhook_git(webhook_id, webhook_token), params: [wait: wait])
  end

  def get_application_information do
    request(:get, Constants.application_information())
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
    with {:ok, body} <- payload, do: {:ok, Poison.decode!(body, as: [apply(as, :p_encode, [])])}
  end

  def handle(payload, as) do
    with {:ok, body} <- payload, do: {:ok, Poison.decode!(body, as: apply(as, :p_encode, []))}
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
      body:
        {:multipart,
         [
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
  @spec get_token() :: String.t()
  def get_token do
    Application.get_env(:nostrum, :token)
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
