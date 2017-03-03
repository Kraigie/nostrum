defmodule Mixcord.Struct.Embed.Field do
  @moduledoc """
  Struct representing a Discord embed field.
  """

  @typedoc "Name of the field"
  @type name :: String.t

  @typedoc "Value of the field"
  @type value :: String.t

  @typedoc "Whether the field should display as inline"
  @type inline :: boolean

  @type t :: %__MODULE__{
    name: name,
    value: value,
    inline: inline
  }

  @derive [Poison.Encoder]
  defstruct [
    :name,
    :value,
    :inline
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
