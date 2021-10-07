defmodule Nostrum.Struct.Event.MessageReactionRemoveAll do
  @moduledoc "Sent when a user explicitly removes all reactions from a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Channel, Guild, Message}

  defstruct [:channel_id, :message_id, :guild_id]

  @typedoc "ID of the channel in which the message resides."
  @type channel_id :: Channel.id()

  @typedoc "ID of the message from which all reactions were removed."
  @type message_id :: Message.id()

  @typedoc "ID of the guild for the message, if applicable."
  @type guild_id :: Guild.id() | nil

  @typedoc "Event sent when a user explicitly removes all reactions from a message"
  @type t :: %__MODULE__{
          channel_id: channel_id,
          message_id: message_id,
          guild_id: guild_id
        }

  @doc false
  def to_struct(map) do
    %__MODULE__{
      channel_id: map.channel_id,
      message_id: map.message_id,
      guild_id: map[:guild_id]
    }
  end
end
