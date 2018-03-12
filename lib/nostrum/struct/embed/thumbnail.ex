defmodule Nostrum.Struct.Embed.Thumbnail do
  @moduledoc """
  Struct representing a Discord embed thumbnail.
  """

  alias Nostrum.Util

  defstruct [
    :url,
    :proxy_url,
    :height,
    :width
  ]

  @typedoc "Source URL of the thumbnail"
  @type url :: String.t | nil

  @typedoc "URL of thumbnail icon"
  @type proxy_url :: String.t | nil

  @typedoc "Height of the thumbnail"
  @type height :: integer | nil

  @typedoc "Width of the thumbnail"
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
