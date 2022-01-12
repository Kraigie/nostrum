defmodule Nostrum.Struct.Event.MessageReactionRemove do
  @moduledoc "Sent when a user removes a reaction from a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Channel, Emoji, Guild, Message, User}
  alias Nostrum.{Snowflake, Util}

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
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:user_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:message_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:emoji, nil, &Util.cast(&1, {:struct, Emoji}))

    struct(__MODULE__, new)
  end
end
