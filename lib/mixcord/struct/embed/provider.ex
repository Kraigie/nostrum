defmodule Nostrum.Struct.Embed.Provider do
  @moduledoc """
  Struct representing a Discord embed provider.
  """

  @typedoc "Name of the provider"
  @type name :: String.t

  @typedoc "URL of provider"
  @type url :: String.t

  @type t :: %__MODULE__{
    name: name,
    url: url
  }

  @derive [Poison.Encoder]
  defstruct [
    :name,
    :url
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
