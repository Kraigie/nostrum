defmodule Nostrum.Struct.Embed.Field do
  @moduledoc """
  Struct representing a Discord embed field.
  """

  alias Nostrum.Util

  defstruct [
    :name,
    :value,
    :inline
  ]

  @typedoc "Name of the field"
  @type name :: String.t | nil

  @typedoc "Value of the field"
  @type value :: String.t | nil

  @typedoc "Whether the field should display as inline"
  @type inline :: boolean | nil

  @type t :: %__MODULE__{
    name: name,
    value: value,
    inline: inline
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end
