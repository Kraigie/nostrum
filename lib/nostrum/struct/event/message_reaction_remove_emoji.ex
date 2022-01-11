defmodule Nostrum.Struct.Event.MessageReactionRemoveEmoji do
  @moduledoc "Sent when a bot removes all instances of a given emoji from the reactions of a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Channel, Emoji, Guild, Message}
  alias Nostrum.{Snowflake, Util}

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
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:message_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:emoji, nil, &Util.cast(&1, {:struct, Emoji}))

    struct(__MODULE__, new)
  end
end
