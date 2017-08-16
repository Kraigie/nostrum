defmodule Nostrum.Struct.Embed do
  @moduledoc """
  Struct representing a Discord embed.
  """

  alias Nostrum.Struct.Embed.{Author, Field, Footer, Image,
    Provider, Thumbnail, Video}
  alias Nostrum.Util

  @typedoc "Title of the embed"
  @type title :: String.t

  @typedoc "Type of the embed"
  @type type :: String.t

  @typedoc "Description of the embed"
  @type description :: String.t

  @typedoc "Url of the embed"
  @type url :: String.t

  @typedoc "Timestamp of embed content"
  @type timestamp :: String.t

  @typedoc "Color code of the embed"
  @type color :: Integer.t

  @typedoc "Footer information"
  @type footer :: Footer.t

  @typedoc "Image information"
  @type image :: Image.t

  @typedoc "Thumbnail information"
  @type thumbnail :: Thumbnail.t

  @typedoc "Video information"
  @type video :: Video.t

  @typedoc "Provider information"
  @type provider :: Provider.t

  @typedoc "Author information"
  @type author :: Author.t

  @typedoc "Fields information"
  @type fields :: [Field.t]

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

  @derive [Poison.Encoder]
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

  # TODO: Jump down the rabbit hole
  def p_encode do
    %__MODULE__{

    }
  end

  @doc false
  def to_struct(map) do
    new = map
    |> Map.update(:footer, %{}, &Footer.to_struct(&1))
    |> Map.update(:image, %{}, &Image.to_struct(&1))
    |> Map.update(:thumbnail, %{}, &Thumbnail.to_struct(&1))
    |> Map.update(:video, %{}, &Video.to_struct(&1))
    |> Map.update(:provider, %{}, &Provider.to_struct(&1))
    |> Map.update(:author, %{}, &Author.to_struct(&1))
    |> Map.update(:fields, [], &Util.list_to_struct_list(&1, Field))
    struct(__MODULE__, new)
  end
end
