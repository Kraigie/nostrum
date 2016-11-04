defmodule Mixcord.Struct.Message do
  @moduledoc """
  Struct representing a Discord message and various helper functions.
  """

  @doc """
  Defines the Message struct.

  * :id - *String*. Id of the message.
  * :channel_id - *String*. Id of the channel the message was sent in.
  * :author -	*Struct*. A `Mixcord.Constructs.User` struct.
  * :content - *String*. Contents of the message.
  * :timestamp - *Date*. When this message was sent.
  * :edited_timestamp - *?Date*. When this message was edited (or null if never).
  * :tts - *Boolean*. Whether this was a TTS message.
  * :mention_everyone - *Boolean*. Whether this message mentions everyone.
  * :mentions -	*List*. A list of `Mixcord.Constructs.User` structs mentioned in the message.
  * :mention_roles - *List*. A list of `Mixcord.Constructs.Role` structs mentioned in this message.
  * :attachments - *List*. A list of [attached files](https://discordapp.com/developers/docs/resources/channel#attachment-object) as maps.
  * :embeds - *List*. A list of [embedded content](https://discordapp.com/developers/docs/resources/channel#embed-object) as maps.
  * :nonce - *?String*. Used for validating a message was sent.
  * :pinned - *Boolean*. Whether this message is pinned.
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