defmodule Nostrum.Struct.Event.PollVoteChange do
  @moduledoc """
  Represents an add/removal of a vote from a Discord poll.

  For polls where multiple answers were selected, one of these events will be fired for each vote.
  """
  alias Nostrum.Util

  defstruct [:user_id, :channel_id, :message_id, :guild_id, :answer_id, :type]

  @typedoc "ID of the user that has voted"
  @type user_id :: integer

  @typedoc "ID of the channel the vote took place in"
  @type channel_id :: integer

  @typedoc "ID of the message the poll was attached to"
  @type message_id :: integer

  @typedoc "ID of the guild the poll is in (unless it is a private channel)"
  @type guild_id :: integer

  @typedoc "ID of the answer index on the poll object that was voted for"
  @type answer_id :: integer

  @typedoc "Whether the vote was an add or removal for the option"
  @type type :: :add | :remove

  @typedoc "Event representing a add/removal of a vote from a poll"
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
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
