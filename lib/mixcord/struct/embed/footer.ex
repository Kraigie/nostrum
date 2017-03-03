defmodule Mixcord.Struct.Embed.Footer do
  @moduledoc """
  Struct representing a Discord embed footer.
  """

  @typedoc "Footer text"
  @type text :: String.t

  @typedoc "URL of footer icon"
  @type icon_url :: String.t

  @typedoc "Proxied URL of footer icon"
  @type proxy_icon_url :: String.t

  @type t :: %__MODULE__{
    text: text,
    icon_url: icon_url,
    proxy_icon_url: proxy_icon_url
  }

  @derive [Poison.Encoder]
  defstruct [
    :text,
    :icon_url,
    :proxy_icon_url
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
