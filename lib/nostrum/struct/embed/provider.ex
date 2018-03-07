defmodule Nostrum.Struct.Embed.Provider do
  @moduledoc """
  Struct representing a Discord embed provider.
  """

  alias Nostrum.Util

  defstruct [
    :name,
    :url
  ]

  @typedoc "Name of the provider"
  @type name :: String.t | nil

  @typedoc "URL of provider"
  @type url :: String.t | nil

  @type t :: %__MODULE__{
    name: name,
    url: url
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end
