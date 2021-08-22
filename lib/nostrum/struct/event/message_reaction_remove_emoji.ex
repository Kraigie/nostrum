defmodule Nostrum.Struct.Event.MessageReactionRemoveEmoji do
  @moduledoc "Sent when a bot removes all instances of a given emoji from the reactions of a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Channel, Emoji, Guild, Message}
  alias Nostrum.Util

  defstruct [:channel_id, :guild_id, :message_id, :emoji]

  @typedoc "Channel in which the message resides."
  @type channel_id :: Channel.id()

  @typedoc "Guild on which the message resides, if applicable."
  @type guild_id :: Guild.id() | nil

  @typedoc "Message from which the emoji was removed."
  @type message_id :: Message.id()

  @typedoc "The (partial) emoji that was removed."
  @type emoji :: Emoji.t()

  @typedoc "Event sent when a bot removes all instances of a given emoji from the reactions of a message"
  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id,
          message_id: message_id,
          emoji: emoji
        }

  @doc false
  def to_struct(map) do
    %__MODULE__{
      channel_id: map.channel_id,
      guild_id: map[:guild_id],
      message_id: map.message_id,
      emoji: Util.cast(map.emoji, {:struct, Emoji})
    }
  end
end
