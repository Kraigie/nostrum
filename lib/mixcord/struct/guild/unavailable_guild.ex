defmodule Mixcord.Struct.Guild.UnavailableGuild do
  @moduledoc """
  Struct representing an unavailable Discord guild.
  """

  @typedoc "The guild's id"
  @type id :: integer

  @typedoc "Whether the guild is avaliable"
  @type unavailable :: boolean

  @type t :: %__MODULE__{
    id: id,
    unavailable: unavailable
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :unavailable
  ]

  def to_struct(map), do: struct(__MODULE__, map)
end
