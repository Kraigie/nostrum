defmodule Nostrum.Struct.Embed do
  @moduledoc """
  Struct representing a Discord embed.
  """

  alias Nostrum.Struct.Embed.{Author, Field, Footer, Image, Provider, Thumbnail, Video}
  alias Nostrum.Util

  defstruct [
    :title,
    :type,
    :description,
    :url,
    :timestamp,
    :color,
    :footer,
    :image,
    :thumbnail,
    :video,
    :provider,
    :author,
    :fields
  ]

  defimpl Poison.Encoder do
    def encode(embed, options) do
      embed
      |> Map.from_struct()
      |> Enum.filter(fn {_, v} -> v != nil end)
      |> Map.new()
      |> Poison.Encoder.encode(options)
    end
  end

  @typedoc "Title of the embed"
  @type title :: String.t() | nil

  @typedoc "Type of the embed"
  @type type :: String.t() | nil

  @typedoc "Description of the embed"
  @type description :: String.t() | nil

  @typedoc "Url of the embed"
  @type url :: String.t() | nil

  @typedoc "Timestamp of embed content"
  @type timestamp :: String.t() | nil

  @typedoc "Color code of the embed"
  @type color :: Integer.t() | nil

  @typedoc "Footer information"
  @type footer :: Footer.t() | nil

  @typedoc "Image information"
  @type image :: Image.t() | nil

  @typedoc "Thumbnail information"
  @type thumbnail :: Thumbnail.t() | nil

  @typedoc "Video information"
  @type video :: Video.t() | nil

  @typedoc "Provider information"
  @type provider :: Provider.t() | nil

  @typedoc "Author information"
  @type author :: Author.t() | nil

  @typedoc "Fields information"
  @type fields :: [Field.t()] | nil

  @type t :: %__MODULE__{
          title: title,
          type: type,
          description: description,
          url: url,
          timestamp: timestamp,
          color: color,
          footer: footer,
          image: image,
          thumbnail: thumbnail,
          video: video,
          provider: provider,
          author: author,
          fields: fields
        }

  # TODO: Jump down the rabbit hole
  def p_encode do
    %__MODULE__{}
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:footer, nil, &Util.cast(&1, {:struct, Footer}))
      |> Map.update(:image, nil, &Util.cast(&1, {:struct, Image}))
      |> Map.update(:thumbnail, nil, &Util.cast(&1, {:struct, Thumbnail}))
      |> Map.update(:video, nil, &Util.cast(&1, {:struct, Video}))
      |> Map.update(:provider, nil, &Util.cast(&1, {:struct, Provider}))
      |> Map.update(:author, nil, &Util.cast(&1, {:struct, Author}))
      |> Map.update(:fields, nil, &Util.cast(&1, {:list, {:struct, Field}}))

    struct(__MODULE__, new)
  end
end
