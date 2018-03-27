defmodule Nostrum.Struct.Message.Reaction do
  @moduledoc """
  Struct representing a Discord message reaction.
  """

  alias Nostrum.Struct.Emoji
  alias Nostrum.Util

  defstruct [
    :count,
    :me,
    :emoji
  ]

  @typedoc "Times this emoji has been used to react"
  @type count :: integer

  @typedoc "Whether the current user is the one who reacted"
  @type me :: boolean

  @typedoc "Emoji information"
  @type emoji :: Emoji.t()

  @type t :: %__MODULE__{
          count: count,
          me: me,
          emoji: emoji
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:emoji, nil, &Util.cast(&1, {:struct, Emoji}))

    struct(__MODULE__, new)
  end
end
