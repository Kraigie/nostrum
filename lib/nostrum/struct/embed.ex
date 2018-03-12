defmodule Nostrum.Struct.Embed do
  @moduledoc """
  Struct representing a Discord embed.

  A `Nostrum.Struct.Embed` is a form of formatting used to make message content 
  look more nice.

  Unlike other kinds of discord objects, all `Nostrum.Struct.Embed` fields are optional. This includes 
  their children objects, such as `Nostrum.Struct.Embed.Author`, `Nostrum.Struct.Embed.Provider`, etc. 
  Thus, nostrum has one condition for a `Nostrum.Struct.Embed` to be valid:

    * Any `Nostrum.Struct.Embed` and its children must have at least one non-nil value

  Breaking this condition means that discord will reject any embeds nostrum sends through the API.
  """

  alias Nostrum.Struct.Embed.{Author, Field, Footer, Image,
    Provider, Thumbnail, Video}
  alias Nostrum.Util

  @typedoc "Title of the embed"
  @type title :: String.t | nil

  @typedoc "Type of the embed"
  @type type :: String.t

  @typedoc "Description of the embed"
  @type description :: String.t | nil

  @typedoc "Url of the embed"
  @type url :: String.t | nil

  @typedoc "Timestamp of embed content"
  @type timestamp :: String.t | nil

  @typedoc "Color code of the embed"
  @type color :: integer | nil

  @typedoc "Footer information"
  @type footer :: Footer.t | nil

  @typedoc "Image information"
  @type image :: Image.t | nil

  @typedoc "Thumbnail information"
  @type thumbnail :: Thumbnail.t | nil

  @typedoc "Video information"
  @type video :: Video.t | nil

  @typedoc "Provider information"
  @type provider :: Provider.t | nil

  @typedoc "Author information"
  @type author :: Author.t | nil

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
    fields: []
  ]

  # TODO: Jump down the rabbit hole
  def p_encode do
    %__MODULE__{

    }
  end

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
    |> Map.update(:footer, nil, &Util.cast_struct(&1, Footer))
    |> Map.update(:image, nil, &Util.cast_struct(&1, Image))
    |> Map.update(:thumbnail, nil, &Util.cast_struct(&1, Thumbnail))
    |> Map.update(:video, nil, &Util.cast_struct(&1, Video))
    |> Map.update(:provider, nil, &Util.cast_struct(&1, Provider))
    |> Map.update(:author, nil, &Util.cast_struct(&1, Author))
    |> Map.update(:fields, [], &Util.cast_struct(&1, Field))
  end
end
