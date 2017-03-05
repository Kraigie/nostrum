defmodule Nostrum.Struct.Embed.Image do
  @moduledoc """
  Struct representing a Discord embed image.
  """

  @typedoc "Image text"
  @type url :: String.t

  @typedoc "URL of image icon"
  @type proxy_url :: String.t

  @typedoc "Height of the image"
  @type height :: integer

  @typedoc "Width of the image"
  @type width :: integer

  @type t :: %__MODULE__{
    url: url,
    proxy_url: proxy_url,
    height: height,
    width: width
  }

  @derive [Poison.Encoder]
  defstruct [
    :url,
    :proxy_url,
    :height,
    :width
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
