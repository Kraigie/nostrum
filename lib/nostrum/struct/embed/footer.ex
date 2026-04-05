defmodule Nostrum.Struct.Embed.Footer do
  @moduledoc """
  Struct representing a Discord embed footer.
  """

  alias Nostrum.Util
  defstruct [
    :text,
    :icon_url,
    :proxy_icon_url
  ]

  defimpl JSON.Encoder do
    def encode(footer, encoder) do
      footer
      |> Map.from_struct()
      |> Map.reject(fn {_, v} -> v == nil end)
      |> JSON.Encoder.Map.encode(encoder)
    end
  end

  @typedoc "Footer text"
  @type text :: String.t()

  @typedoc "URL of footer icon"
  @type icon_url :: String.t() | nil

  @typedoc "Proxied URL of footer icon"
  @type proxy_icon_url :: String.t() | nil

  @type t :: %__MODULE__{
          text: text,
          icon_url: icon_url,
          proxy_icon_url: proxy_icon_url
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
