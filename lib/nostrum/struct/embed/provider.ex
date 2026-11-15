defmodule Nostrum.Struct.Embed.Provider do
  @moduledoc """
  Struct representing a Discord embed provider.
  """

  alias Nostrum.Util
  defstruct [
    :name,
    :url
  ]

  defimpl JSON.Encoder do
    def encode(provider, encoder) do
      provider
      |> Map.from_struct()
      |> Map.reject(fn {_, v} -> v == nil end)
      |> JSON.Encoder.Map.encode(encoder)
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
