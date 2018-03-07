defmodule Nostrum.Struct.Embed.Image do
  @moduledoc """
  Struct representing a Discord embed image.
  """

  alias Nostrum.Util

  defstruct [
    :url,
    :proxy_url,
    :height,
    :width
  ]

  @typedoc "Image text"
  @type url :: String.t | nil

  @typedoc "URL of image icon"
  @type proxy_url :: String.t | nil

  @typedoc "Height of the image"
  @type height :: integer | nil

  @typedoc "Width of the image"
  @type width :: integer | nil

  @type t :: %__MODULE__{
    url: url,
    proxy_url: proxy_url,
    height: height,
    width: width
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end
