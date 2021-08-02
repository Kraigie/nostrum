defmodule Nostrum.Struct.Event.MessageDelete do
  @moduledoc """
  Struct representing a Message Delete event
  """

  alias Nostrum.Struct.{Channel, Guild, Message}

  defstruct [
    :id,
    :channel_id,
    :guild_id
  ]

  @typedoc "Id of the deleted message"
  @type id :: Message.id()

  @typedoc "Channel id of the deleted message"
  @type channel_id :: Channel.id()

  @typedoc """
  Guild id of the deleted message

  `nil` if a non-guild message was deleted.
  """
  @type guild_id :: Guild.id() | nil

  @type t :: %__MODULE__{
          id: id,
          channel_id: channel_id,
          guild_id: guild_id
        }

  @doc false
  def to_struct(map) do
    %__MODULE__{
      id: map["id"],
      channel_id: map["channel_id"],
      # https://github.com/discord/discord-api-docs/issues/296
      guild_id: map.guild_id
    }
  end
end
