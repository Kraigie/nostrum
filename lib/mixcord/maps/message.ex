defmodule Mixcord.Map.Message do
  @moduledoc """
  Struct representing a Discord message.
  """

  alias Mixcord.Map.{Role, User}

  @typedoc "The id of the message"
  @type id :: integer

  @typedoc "The id of the channel"
  @type channel_id :: integer

  @typedoc "The user struct of the author"
  @type author :: User.t

  @typedoc "The content of the message"
  @type content :: String.t

  @typedoc "When the message was sent"
  @type timestamp :: String.t

  @typedoc "When the message was edited"
  @type edited_timestamp :: String.t | nil

  @typedoc "Whether this was a TTS message"
  @type tts :: boolean

  @typedoc "Whether this messsage mentions everyone"
  @type mention_everyone :: boolean

  @typedoc "List of users mentioned in the message"
  @type mentions :: list(User.t)

  @typedoc "List of roles mentioned in the message"
  @type mention_roles :: list(Role.t)

  @typedoc "List of attached files in the message"
  @type attachments :: list(Map.t)

  @typedoc "List of embedded content in the message"
  @type embeds :: list(Map.t)

  @typedoc "Validates if a message was sent"
  @type nonce :: String.t

  @typedoc "Whether this message is pinned"
  @type pinned :: boolean

  @type t :: Map.t

  @doc """
  Represents a Discord Message.

  * `:id` - *Integer*. Id of the message.
  * `:channel_id` - *Integer*. Id of the channel the message was sent in.
  * `:author` - *Struct*. A `Mixcord.Map.User` struct.
  * `:content` - *String*. Contents of the message.
  * `:timestamp` - *Date*. When this message was sent.
  * `:edited_timestamp` - *?Date*. When this message was edited (or null if never).
  * `:tts` - *Boolean*. Whether this was a TTS message.
  * `:mention_everyone` - *Boolean*. Whether this message mentions everyone.
  * `:mentions` - *List*. A list of `Mixcord.Map.User` maps mentioned in the message.
  * `:mention_roles` - *List*. A list of `Mixcord.Map.Role` maps mentioned in this message.
  * `:attachments` - *List*. A list of [attached files](https://discordapp.com/developers/docs/resources/channel#attachment-object) as maps.
  * `:embeds` - *List*. A list of [embedded content](https://discordapp.com/developers/docs/resources/channel#embed-object) as maps.
  * `:nonce` - *?String*. Used for validating a message was sent.
  * `:pinned` - *Boolean*. Whether this message is pinned.
  """
  @derive [Poison.Encoder]
  defstruct [
    :attachments,
    :author,
    :channel_id,
    :content,
    :edited_timestamp,
    :embeds,
    :id,
    :mention_everyone,
    :mention_roles,
    :mentions,
    :nonce,
    :pinned,
    :timestamp,
    :tts,
    :type,
  ]
end
