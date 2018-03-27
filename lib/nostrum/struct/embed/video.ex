defmodule Nostrum.Struct.Embed.Video do
  @moduledoc """
  Struct representing a Discord embed video.
  """

  @typedoc "Source URL of the video"
  @type url :: String.t()

  @typedoc "Height of the video"
  @type height :: integer

  @typedoc "Width of the video"
  @type width :: integer

  @type t :: %__MODULE__{
          url: url,
          height: height,
          width: width
        }

  @derive [Poison.Encoder]
  defstruct [
    :url,
    :height,
    :width
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
