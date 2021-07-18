defmodule Nostrum.Struct.Event.MessageReactionRemove do
  @moduledoc "Sent when a user removes a reaction from a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Channel, Emoji, Guild, Message, User}
  alias Nostrum.Util

  defstruct [:user_id, :channel_id, :message_id, :guild_id, :emoji]

  # XXX: is this correct?
  @typedoc "Author of the reaction"
  @type user_id :: User.id()

  @typedoc "ID of the channel in which the reaction was created"
  @type channel_id :: Channel.id()

  @typedoc "ID of the message to which the reaction was attached"
  @type message_id :: Message.id()

  @typedoc "ID of the guild on which the message lives, if applicable"
  @type guild_id :: Guild.id() | nil

  @typedoc "Partial emoji object that was removed"
  @type emoji :: Emoji.t() | nil

  @typedoc "Event sent when a user removes a reaction from a message"
  @type t :: %__MODULE__{
          user_id: user_id,
          channel_id: channel_id,
          message_id: message_id,
          guild_id: guild_id,
          emoji: emoji
        }

  @doc false
  def to_struct(map) do
    %__MODULE__{
      user_id: map["user_id"],
      channel_id: map["channel_id"],
      message_id: map["message_id"],
      guild_id: map["guild_id"],
      emoji: Util.cast(map["emoji"], {:struct, Emoji})
    }
  end
end
