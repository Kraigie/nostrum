defmodule Nostrum.Struct.Embed.Thumbnail do
  @moduledoc """
  Struct representing a Discord embed thumbnail.
  """

  @typedoc "Source URL of the thumbnail"
  @type url :: String.t

  @typedoc "URL of thumbnail icon"
  @type proxy_url :: String.t

  @typedoc "Height of the thumbnail"
  @type height :: integer

  @typedoc "Width of the thumbnail"
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
