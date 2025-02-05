defmodule Nostrum.Api.Message do
  @moduledoc """
  Module for interacting with the Discord API's message endpoints.

  See: https://discord.com/developers/docs/resources/message
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Message
  alias Nostrum.Struct.User

  import Nostrum.Snowflake, only: [is_snowflake: 1]

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
    Nostrum.Api.Message.create(
      msg.channel_id,
      content: "Hello",
      message_reference: %{message_id: msg.id}
    )
  end
  ```

  Passing a list will merge the settings provided

  ## Examples

  ```elixir
  Nostrum.Api.Message.create(43189401384091, content: "hello world!")

  Nostrum.Api.Message.create(43189401384091, "hello world!")

  import Nostrum.Struct.Embed
  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("embed")
    |> put_description("new desc")
  Nostrum.Api.Message.create(43189401384091, embeds: [embed])

  Nostrum.Api.Message.Message.create(43189401384091, file: "/path/to/file.txt")

  Nostrum.Api.Message.create(43189401384091, content: "hello world!", embeds: [embed], file: "/path/to/file.txt")

  Nostrum.Api.Message.create(43189401384091, content: "Hello @everyone", allowed_mentions: :none)
  ```
  """
  @spec create(Channel.id() | Message.t(), Api.options() | String.t()) ::
          Api.error() | {:ok, Message.t()}
  def create(channel_id, options)

  def create(%Message{} = message, options),
    do: create(message.channel_id, options)

  def create(channel_id, content) when is_binary(content),
    do: create(channel_id, %{content: content})

  def create(channel_id, options) when is_list(options),
    do: create(channel_id, Map.new(options))

  def create(channel_id, options) when is_snowflake(channel_id) and is_map(options) do
    prepared_options =
      Helpers.prepare_allowed_mentions(options)
      |> Helpers.combine_embeds()
      |> Helpers.combine_files()

    Api.request(:post, Constants.channel_messages(channel_id), prepared_options)
    |> Helpers.handle_request_with_decode({:struct, Message})
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
  Nostrum.Api.Message.react(123123123123, 321321321321, emoji)

  # Using a base 16 emoji string.
  Nostrum.Api.Message.react(123123123123, 321321321321, "\xF0\x9F\x98\x81")

  ```

  For other emoji string examples, see `t:Nostrum.Struct.Emoji.api_name/0`.
  """
  @spec react(Channel.id(), Message.id(), Api.emoji()) :: Api.error() | {:ok}
  def react(channel_id, message_id, emoji)

  def react(channel_id, message_id, %Emoji{} = emoji),
    do: react(channel_id, message_id, Emoji.api_name(emoji))

  def react(channel_id, message_id, emoji_api_name) do
    Api.request(:put, Constants.channel_reaction_me(channel_id, message_id, emoji_api_name))
  end

  @doc ~S"""
  Deletes all reactions from a message.

  This endpoint requires the `VIEW_CHANNEL`, `READ_MESSAGE_HISTORY`, and
  `MANAGE_MESSAGES` permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove_all/0` event.

  If successful, returns `{:ok}`. Otherwise, return `t:Nostrum.Api.error/0`.
  """
  @spec clear_reactions(Channel.id(), Message.id()) :: Api.error() | {:ok}
  def clear_reactions(channel_id, message_id) do
    Api.request(:delete, Constants.channel_reactions_delete(channel_id, message_id))
  end

  @doc ~S"""
  Same as `delete/2`, but takes a `Nostrum.Struct.Message` instead of a
  `channel_id` and `message_id`.
  """
  @spec delete(Message.t()) :: Api.error() | {:ok}
  def delete(%Message{id: id, channel_id: c_id}) do
    delete(c_id, id)
  end

  @doc ~S"""
  Deletes a message.

  This endpoint requires the 'VIEW_CHANNEL' and 'MANAGE_MESSAGES' permission. It
  fires the `MESSAGE_DELETE` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Message.delete(43189401384091, 43189401384091)
  ```
  """
  @spec delete(Channel.id(), Message.id()) :: Api.error() | {:ok}
  def delete(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Api.request(:delete, Constants.channel_message(channel_id, message_id))
  end

  @doc ~S"""
  Deletes a reaction the current user has made for the message.

  This endpoint requires the `VIEW_CHANNEL` and `READ_MESSAGE_HISTORY`
  permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `react/3` for similar examples.
  """
  @spec unreact(Channel.id(), Message.id(), Api.emoji()) :: Api.error() | {:ok}
  def unreact(channel_id, message_id, emoji)

  def unreact(channel_id, message_id, %Emoji{} = emoji),
    do: unreact(channel_id, message_id, Emoji.api_name(emoji))

  def unreact(channel_id, message_id, emoji_api_name) do
    Api.request(:delete, Constants.channel_reaction_me(channel_id, message_id, emoji_api_name))
  end

  @doc ~S"""
  Deletes all reactions of a given emoji from a message.

  This endpoint requires the `MANAGE_MESSAGES` permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove_emoji/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `react/3` for similar examples.
  """
  @spec delete_emoji_reactions(Channel.id(), Message.id(), Api.emoji()) :: Api.error() | {:ok}
  def delete_emoji_reactions(channel_id, message_id, emoji)

  def delete_emoji_reactions(channel_id, message_id, %Emoji{} = emoji),
    do: delete_emoji_reactions(channel_id, message_id, Emoji.api_name(emoji))

  def delete_emoji_reactions(channel_id, message_id, emoji_api_name) do
    Api.request(
      :delete,
      Constants.channel_reactions_delete_emoji(channel_id, message_id, emoji_api_name)
    )
  end

  @doc ~S"""
  Deletes another user's reaction from a message.

  This endpoint requires the `VIEW_CHANNEL`, `READ_MESSAGE_HISTORY`, and
  `MANAGE_MESSAGES` permissions. It fires a `t:Nostrum.Consumer.message_reaction_remove/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  See `react/3` for similar examples.
  """
  @spec delete_user_reaction(Channel.id(), Message.id(), Api.emoji(), User.id()) ::
          Api.error() | {:ok}
  def delete_user_reaction(channel_id, message_id, emoji, user_id)

  def delete_user_reaction(channel_id, message_id, %Emoji{} = emoji, user_id),
    do: delete_user_reaction(channel_id, message_id, Emoji.api_name(emoji), user_id)

  def delete_user_reaction(channel_id, message_id, emoji_api_name, user_id) do
    Api.request(
      :delete,
      Constants.channel_reaction(channel_id, message_id, emoji_api_name, user_id)
    )
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
  Nostrum.Api.Message.edit(43189401384091, 1894013840914098, content: "hello world!")

  Nostrum.Api.Message.edit(43189401384091, 1894013840914098, "hello world!")

  import Nostrum.Struct.Embed
  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("embed")
    |> put_description("new desc")
  Nostrum.Api.Message.edit(43189401384091, 1894013840914098, embeds: [embed])

  Nostrum.Api.Message.edit(43189401384091, 1894013840914098, content: "hello world!", embeds: [embed])
  ```
  """
  @spec edit(Channel.id(), Message.id(), Api.options() | String.t()) ::
          Api.error() | {:ok, Message.t()}
  def edit(channel_id, message_id, options)

  def edit(channel_id, message_id, content) when is_binary(content),
    do: edit(channel_id, message_id, %{content: content})

  def edit(channel_id, message_id, options) when is_list(options),
    do: edit(channel_id, message_id, Map.new(options))

  def edit(channel_id, message_id, %{} = options)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    prepared_options =
      Helpers.prepare_allowed_mentions(options)
      |> Helpers.combine_embeds()
      |> Helpers.combine_files()

    Api.request(:patch, Constants.channel_message(channel_id, message_id), prepared_options)
    |> Helpers.handle_request_with_decode({:struct, Message})
  end

  @doc ~S"""
  Same as `edit/3`, but takes a `Nostrum.Struct.Message` instead of a
  `channel_id` and `message_id`.
  """
  @spec edit(Message.t(), Api.options()) :: Api.error() | {:ok, Message.t()}
  def edit(%Message{id: id, channel_id: c_id}, options) do
    edit(c_id, id, options)
  end

  @doc ~S"""
  Retrieves a message from a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'READ_MESSAGE_HISTORY' permissions.

  If successful, returns `{:ok, message}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Message.get(43189401384091, 198238475613443)
  ```
  """
  @spec get(Channel.id(), Message.id()) :: Api.error() | {:ok, Message.t()}
  def get(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Api.request(:get, Constants.channel_message(channel_id, message_id))
    |> Helpers.handle_request_with_decode({:struct, Message})
  end

  @doc ~S"""
  Gets all users who reacted with an emoji.

  This endpoint requires the `VIEW_CHANNEL` and `READ_MESSAGE_HISTORY` permissions.

  If successful, returns `{:ok, users}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  The optional `params` are `after`, the user ID to query after, absent by default,
  and `limit`, the max number of users to return, 1-100, 25 by default.

  See `react/3` for similar examples.
  """
  @spec reactions(Channel.id(), Message.id(), Api.emoji(), keyword()) ::
          Api.error() | {:ok, [User.t()]}
  def reactions(channel_id, message_id, emoji, params \\ [])

  def reactions(channel_id, message_id, %Emoji{} = emoji, params),
    do: reactions(channel_id, message_id, Emoji.api_name(emoji), params)

  def reactions(channel_id, message_id, emoji_api_name, params) do
    Api.request(
      :get,
      Constants.channel_reactions_get(channel_id, message_id, emoji_api_name),
      "",
      params
    )
    |> Helpers.handle_request_with_decode({:list, {:struct, User}})
  end
end
