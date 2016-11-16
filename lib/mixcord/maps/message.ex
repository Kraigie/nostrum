defmodule Mixcord.Map.Message do
  @moduledoc """
  Struct representing a Discord message.
  """

  alias Mixcord.Map.{Role, User}

  @type id :: integer
  @type channel_id :: integer
  @type author :: User.t
  @type content :: String.t
  @type timestamp :: String.t
  @type edited_timestamp :: String.t
  @type tts :: boolean
  @type mention_everyone :: boolean
  @type mentions :: list(User.t)
  @type mention_roles :: list(Role.t)
  @type attachments :: list(Map.t)
  @type embeds :: list(Map.t)
  @type nonce :: String.t
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
