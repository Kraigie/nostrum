defmodule Nostrum.Struct.Embed do
  @moduledoc ~S"""
  Functions that work on Discord embeds.

  ## Building Embeds

  `Nostrum.Struct.Embed`s can be built using this module's builder functions:

  ```Elixir
  import Nostrum.Struct.Embed

  embed =
    %Nostrum.Struct.Embed{}
    |> put_title("craig")
    |> put_description("nostrum")
    |> put_url("https://google.com/")
    |> put_timestamp("2016-05-05T21:04:13.203Z")
    |> put_color(431_948)
    |> add_field("Field 1", "Test")
    |> add_field("Field 2", "More test", true)
  ```

  Alternatively, it is possible to build `Nostrum.Struct.Embed`s using standard map syntax.
  However, we recommend sticking to the aforementioned builder functions.

  ```Elixir
  embed = %Nostrum.Struct.Embed{
    title: "craig",
    description: "nostrum",
    url: "https://google.com/",
    timestamp: "2016-05-05T21:04:13.203Z",
    color: 431_948,
    fields: [
      %Nostrum.Struct.Embed.Field{name: "Field 1", value: "Test"},
      %Nostrum.Struct.Embed.Field{name: "Field 2", value: "More test", inline: true}
    ]
  }
  ```
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

  @doc ~S"""
  Puts the given `value` under `:title` in `embed`.
  """
  @spec put_title(t, title) :: t
  def put_title(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | title: value}
  end

  @doc false
  @spec put_type(t, type) :: t
  def put_type(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | type: value}
  end

  @doc ~S"""
  Puts the given `value` under `:description` in `embed`.
  """
  @spec put_description(t, description) :: t
  def put_description(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | description: value}
  end

  @doc ~S"""
  Puts the given `value` under `:url` in `embed`.
  """
  @spec put_url(t, url) :: t
  def put_url(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | url: value}
  end

  @doc ~S"""
  Puts the given `value` under `:timestamp` in `embed`.
  """
  @spec put_timestamp(t, timestamp) :: t
  def put_timestamp(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | timestamp: value}
  end

  @doc ~S"""
  Puts the given `value` under `:color` in `embed`.
  """
  @spec put_color(t, color) :: t
  def put_color(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | color: value}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Footer` under `:footer` in `embed`.
  """
  @spec put_footer(t, Footer.text(), Footer.icon_url()) :: t
  def put_footer(%__MODULE__{} = embed, text, icon_url) do
    footer = %Footer{
      text: text,
      icon_url: icon_url
    }

    %__MODULE__{embed | footer: footer}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Image` under `:image` in `embed`.
  """
  @spec put_image(t, Image.url()) :: t
  def put_image(%__MODULE__{} = embed, url) do
    image = %Image{
      url: url
    }

    %__MODULE__{embed | image: image}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Thumbnail` under `:thumbnail` in `embed`.
  """
  @spec put_thumbnail(t, Thumbnail.url()) :: t
  def put_thumbnail(%__MODULE__{} = embed, url) do
    thumbnail = %Thumbnail{
      url: url
    }

    %__MODULE__{embed | thumbnail: thumbnail}
  end

  @doc false
  @spec put_video(t, Video.url()) :: t
  def put_video(%__MODULE__{} = embed, url) do
    video = %Video{
      url: url
    }

    %__MODULE__{embed | video: video}
  end

  @doc false
  @spec put_provider(t, Provider.name(), Provider.url()) :: t
  def put_provider(%__MODULE__{} = embed, name, url) do
    provider = %Provider{
      name: name,
      url: url
    }

    %__MODULE__{embed | provider: provider}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Author` under `:author` in `embed`.
  """
  @spec put_author(t, Author.name(), Author.url(), Author.icon_url()) :: t
  def put_author(%__MODULE__{} = embed, name, url, icon_url) do
    author = %Author{
      name: name,
      url: url,
      icon_url: icon_url
    }

    %__MODULE__{embed | author: author}
  end

  @doc ~S"""
  Adds a `Nostrum.Struct.Embed.Field` under `:fields` in `embed`.
  """
  @spec add_field(t, Field.name(), Field.value(), Field.inline()) :: t
  def add_field(embed, name, value, inline \\ nil)

  def add_field(%__MODULE__{fields: fields} = embed, name, value, inline) when is_list(fields) do
    field = %Field{
      name: name,
      value: value,
      inline: inline
    }

    %__MODULE__{embed | fields: fields ++ [field]}
  end

  def add_field(embed, name, value, inline) do
    add_field(%__MODULE__{embed | fields: []}, name, value, inline)
  end

  # TODO: Jump down the rabbit hole
  @doc false
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
