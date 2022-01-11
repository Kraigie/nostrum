defmodule Nostrum.Struct.Event.MessageReactionAdd do
  @moduledoc "Sent when a user adds a reaction to a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.{Channel, Emoji, Guild, Message, User}
  alias Nostrum.{Snowflake, Util}

  defstruct [:user_id, :channel_id, :message_id, :guild_id, :member, :emoji]

  @typedoc "ID of the user who added the reaction"
  @type user_id :: User.id()

  @typedoc "Channel in which the reaction was added"
  @type channel_id :: Channel.id()

  @typedoc "Message to which the reaction was added"
  @type message_id :: Message.id()

  @typedoc "Guild ID in which the reaction was added, if applicable"
  @type guild_id :: Guild.id() | nil

  @typedoc "The member who reacted, if this happened on a guild"
  @type member :: Member.t() | nil

  @typedoc "The (partial) emoji used to react"
  @type emoji :: Emoji.t()

  @typedoc "Event sent when a user adds a reaction to a message"
  @type t :: %__MODULE__{
          user_id: user_id,
          channel_id: channel_id,
          message_id: message_id,
          guild_id: guild_id,
          member: member,
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
      |> Map.update(:member, nil, &Util.cast(&1, {:struct, Member}))
      |> Map.update(:emoji, nil, &Util.cast(&1, {:struct, Emoji}))

    struct(__MODULE__, new)
  end
end
