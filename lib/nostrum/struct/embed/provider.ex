defmodule Nostrum.Struct.Embed.Provider do
  @moduledoc """
  Struct representing a Discord embed provider.
  """

  alias Nostrum.Util
  alias Jason.{Encode, Encoder}

  defstruct [
    :name,
    :url
  ]

  defimpl Encoder do
    def encode(provider, options) do
      provider
      |> Map.from_struct()
      |> Enum.filter(fn {_, v} -> v != nil end)
      |> Map.new()
      |> Encode.map(options)
    end
  end

  @typedoc "Name of the provider"
  @type name :: String.t() | nil

  @typedoc "URL of provider"
  @type url :: String.t() | nil

  @type t :: %__MODULE__{
          name: name,
          url: url
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
