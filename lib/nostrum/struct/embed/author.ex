defmodule Nostrum.Struct.Embed.Author do
  @moduledoc """
  Struct representing a Discord embed author.
  """

  alias Nostrum.Util

  defstruct [
    :name,
    :url,
    :icon_url,
    :proxy_icon_url
  ]

  @typedoc "Name of the author"
  @type name :: String.t | nil

  @typedoc "URL of the author"
  @type url :: String.t | nil

  @typedoc "URL of the author icon"
  @type icon_url :: String.t | nil

  @typedoc "Proxied URL of author icon"
  @type proxy_icon_url :: String.t | nil

  @type t :: %__MODULE__{
    name: name,
    url: url,
    icon_url: icon_url,
    proxy_icon_url: proxy_icon_url
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end
