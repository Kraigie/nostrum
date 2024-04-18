defmodule Nostrum.Struct.Event.PollVoteChange do
  @moduledoc """
  Represents an addition or removal of a vote from a Discord poll.

  For polls where multiple answers were selected, one of these events will be fired for each vote.
  """
  alias Nostrum.Util

  alias Nostrum.Struct.{Channel, Guild, Message, User}

  defstruct [:user_id, :channel_id, :message_id, :guild_id, :answer_id, :type]

  @typedoc "ID of the user that has voted"
  @type user_id :: User.id()

  @typedoc "ID of the channel the vote took place in"
  @type channel_id :: Channel.id()

  @typedoc "ID of the message the poll was attached to"
  @type message_id :: Message.id()

  @typedoc "ID of the guild the poll is in (unless it is a private channel)"
  @type guild_id :: Guild.id()

  @typedoc "ID corresponding to the answer_id in the `t:Nostrum.Struct.Message.Poll.answers/0` list"
  @type answer_id :: integer

  @typedoc "Whether the vote was an addition or removal for a vote of the option"
  @type type :: :add | :remove

  @typedoc "Event representing a addition or removal of a vote from a poll"
  @type t :: %__MODULE__{
          user_id: user_id,
          channel_id: channel_id,
          message_id: message_id,
          guild_id: guild_id,
          answer_id: answer_id,
          type: type
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
