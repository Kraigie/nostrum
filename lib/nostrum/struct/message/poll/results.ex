defmodule Nostrum.Struct.Message.Poll.Results do
  @moduledoc """
  A struct representing the results of a poll.
  """

  alias Nostrum.Util

  defstruct [
    :is_finalized,
    :answer_counts
  ]

  @typedoc """
  A flag on whether the poll has finished counting.

  If this is set to true, the counts are guaranteed to be accurate from Discord.
  """
  @type is_finalized :: boolean

  @typedoc """
  A list of objects representing the counts for each of the potential answers in the poll.

  The `id` property of each item corresponds to the `answer_id`
  of the poll answers. If an option is not present in this list, then
  there were no votes for that answer.
  """
  @type answer_counts :: [%{id: integer, count: integer, me_voted: boolean}]

  @type t :: %__MODULE__{
          is_finalized: is_finalized,
          answer_counts: answer_counts
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
