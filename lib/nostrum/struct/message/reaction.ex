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
  @type emoji :: Emoji.t

  @type t :: %__MODULE__{
    count: count,
    me: me,
    emoji: emoji,
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
    |> Map.update(:emoji, nil, &Emoji.to_struct/1)
  end
end
