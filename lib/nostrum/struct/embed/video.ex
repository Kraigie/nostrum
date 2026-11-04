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

  defimpl JSON.Encoder do
    def encode(video, encoder) do
      video
      |> Map.from_struct()
      |> Map.reject(fn {_, v} -> v == nil end)
      |> JSON.Encoder.Map.encode(encoder)
    end
  end

  @typedoc "Source URL of the video"
  @type url :: String.t() | nil

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
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
