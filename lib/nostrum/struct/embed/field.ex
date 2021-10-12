defmodule Nostrum.Struct.Embed.Field do
  @moduledoc """
  Struct representing a Discord embed field.
  """

  alias Nostrum.Util
  alias Jason.{Encode, Encoder}

  defstruct [
    :name,
    :value,
    :inline
  ]

  defimpl Encoder do
    def encode(field, options) do
      field
      |> Map.from_struct()
      |> Enum.filter(fn {_, v} -> v != nil end)
      |> Map.new()
      |> Encode.map(options)
    end
  end

  @typedoc "Name of the field"
  @type name :: String.t()

  @typedoc "Value of the field"
  @type value :: String.t()

  @typedoc "Whether the field should display as inline"
  @type inline :: boolean | nil

  @type t :: %__MODULE__{
          name: name,
          value: value,
          inline: inline
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
