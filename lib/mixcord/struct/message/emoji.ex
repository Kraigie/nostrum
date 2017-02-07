defmodule Mixcord.Struct.Emoji do
  @moduledoc """
  Struct representing a Discord emoji.
  """

  @typedoc "Id of the emoji"
  @type id :: integer

  @typedoc "Name of the emoji"
  @type name :: String.t

  @type t :: %__MODULE__{
    id: id,
    name: name
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name
  ]
end
