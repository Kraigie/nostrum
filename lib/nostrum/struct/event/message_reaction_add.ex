defmodule Nostrum.Struct.Event.MessageReactionAdd do
  @moduledoc "Sent when a user adds a reaction to a message"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.{Channel, Emoji, Guild, Message, User}
  alias Nostrum.Util

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
    %__MODULE__{
      user_id: map.user_id,
      channel_id: map.channel_id,
      message_id: map.message_id,
      guild_id: map[:guild_id],
      member: Util.cast(map[:member], {:struct, Member}),
      emoji: Util.cast(map.emoji, {:struct, Emoji})
    }
  end
end
