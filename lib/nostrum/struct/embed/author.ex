defmodule Nostrum.Struct.Embed.Author do
  @moduledoc """
  Struct representing a Discord embed author.
  """

  alias Nostrum.Util
  alias Jason.{Encode, Encoder}

  defstruct [
    :name,
    :url,
    :icon_url,
    :proxy_icon_url
  ]

  defimpl Encoder do
    def encode(author, options) do
      author
      |> Map.from_struct()
      |> Enum.filter(fn {_, v} -> v != nil end)
      |> Map.new()
      |> Encode.map(options)
    end
  end

  @typedoc "Name of the author"
  @type name :: String.t() | nil

  @typedoc "URL of the author"
  @type url :: String.t() | nil

  @typedoc "URL of the author icon"
  @type icon_url :: String.t() | nil

  @typedoc "Proxied URL of author icon"
  @type proxy_icon_url :: String.t() | nil

  @type t :: %__MODULE__{
          name: name,
          url: url,
          icon_url: icon_url,
          proxy_icon_url: proxy_icon_url
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
