defmodule Nostrum.Struct.Event.MessageReactionRemoveAll do
  @moduledoc "Sent when a user explicitly removes all reactions from a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Channel, Guild, Message}
  alias Nostrum.{Snowflake, Util}

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
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:message_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
