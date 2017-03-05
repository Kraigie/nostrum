defmodule Nostrum.Struct.Embed.Author do
  @moduledoc """
  Struct representing a Discord embed author.
  """

  @typedoc "Name of the author"
  @type name :: String.t

  @typedoc "URL of the author"
  @type url :: String.t

  @typedoc "URL of the author icon"
  @type icon_url :: String.t

  @typedoc "Proxied URL of author icon"
  @type proxy_icon_url :: String.t

  @type t :: %__MODULE__{
    name: name,
    url: url,
    icon_url: icon_url,
    proxy_icon_url: proxy_icon_url
  }

  @derive [Poison.Encoder]
  defstruct [
    :name,
    :url,
    :icon_url,
    :proxy_icon_url
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
