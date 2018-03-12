defmodule Nostrum.Struct.Embed.Video do
  @moduledoc """
  Struct representing a Discord embed video.
  """

  alias Nostrum.Util

  defstruct [
    :url,
    :height,
    :width
  ]

  @typedoc "Source URL of the video"
  @type url :: String.t | nil

  @typedoc "Height of the video"
  @type height :: integer | nil

  @typedoc "Width of the video"
  @type width :: integer | nil

  @type t :: %__MODULE__{
    url: url,
    height: height,
    width: width
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end
