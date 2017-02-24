defmodule Mixcord.Struct.Message.Reaction do
  @moduledoc """
  Struct representing a Discord message reaction.
  """

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

  @derive [Poison.Encoder]
  defstruct [
    :count,
    :me,
    :emoji
  ]

  @doc false
  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
