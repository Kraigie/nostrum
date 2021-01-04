defmodule Nostrum.Struct.Embed do
  @moduledoc ~S"""
  Functions that work on Discord embeds.

  ## Building Embeds

  `Nostrum.Struct.Embed`s can be built using this module's builder functions
  or standard `Map` syntax:

  ```Elixir
  iex> import Nostrum.Struct.Embed
  ...> embed =
  ...>   %Nostrum.Struct.Embed{}
  ...>   |> put_title("craig")
  ...>   |> put_description("nostrum")
  ...>   |> put_url("https://google.com/")
  ...>   |> put_timestamp("2016-05-05T21:04:13.203Z")
  ...>   |> put_color(431_948)
  ...>   |> put_field("Field 1", "Test")
  ...>   |> put_field("Field 2", "More test", true)
  ...> embed
  %Nostrum.Struct.Embed{
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

  ## Using structs

  You can also create `Nostrum.Struct.Embed`s from structs, by using the
  `Nostrum.Struct.Embed` module. Here's how the example above could be build using structs

  ```Elixir
    defmodule MyApp.MyStruct do
      use Nostrum.Struct.Embed

      defstruct []

      def title(_), do: "craig"
      def description(_), do: "nostrum"
      def url(_), do: "https://google.com/"
      def timestamp(_), do: "2016-05-05T21:04:13.203Z"
      def color(_), do: 431_948

      def fields(_) do
        [
          %Nostrum.Struct.Embed.Field{name: "Field 1", value: "Test"},
          %Nostrum.Struct.Embed.Field{name: "Field 2", value: "More test", inline: true}
        ]
      end
    end

  iex> Nostrum.Struct.Embed.from(%MyApp.MyStruct{})
  %Nostrum.Struct.Embed{
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
  See this modules callbacks for a list of all the functions that can be implemented.

  The implementation of these callbacks is optional. Not implemented functions will simply
  be ignored.
  """

  alias Nostrum.Struct.Embed.{Author, Field, Footer, Image, Provider, Thumbnail, Video}
  alias Nostrum.Util
  alias Poison.Encoder

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

  defimpl Encoder do
    def encode(embed, options) do
      embed
      |> Map.from_struct()
      |> Enum.filter(fn {_, v} -> v != nil end)
      |> Map.new()
      |> Encoder.encode(options)
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
  @type color :: integer() | nil

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

  @callback author(struct) :: author()
  @callback color(struct) :: integer() | nil
  @callback fields(struct) :: fields()
  @callback description(struct) :: description()
  @callback footer(struct) :: footer()
  @callback image(struct) :: url()
  @callback thumbnail(struct) :: url()
  @callback timestamp(struct) :: timestamp()
  @callback title(struct) :: title()
  @callback url(struct) :: url()

  defmacro __using__(_) do
    quote do
      @behaviour Nostrum.Struct.Embed

      def author(_), do: nil
      def color(_), do: nil
      def fields(_), do: nil
      def description(_), do: nil
      def footer(_), do: nil
      def image(_), do: nil
      def thumbnail(_), do: nil
      def timestamp(_), do: nil
      def title(_), do: nil
      def url(_), do: nil

      defoverridable(
        author: 1,
        color: 1,
        fields: 1,
        description: 1,
        footer: 1,
        image: 1,
        thumbnail: 1,
        timestamp: 1,
        title: 1,
        url: 1
      )
    end
  end

  @doc ~S"""
  Puts the given `value` under `:title` in `embed`.

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_title(embed, "nostrum")
  %Nostrum.Struct.Embed{title: "nostrum"}
  ```
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

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_description(embed, "An elixir library for the discord API.")
  %Nostrum.Struct.Embed{description: "An elixir library for the discord API."}
  ```
  """
  @spec put_description(t, description) :: t
  def put_description(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | description: value}
  end

  @doc ~S"""
  Puts the given `value` under `:url` in `embed`.

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_url(embed, "https://github.com/Kraigie/nostrum")
  %Nostrum.Struct.Embed{url: "https://github.com/Kraigie/nostrum"}
  ```
  """
  @spec put_url(t, url) :: t
  def put_url(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | url: value}
  end

  @doc ~S"""
  Puts the given `value` under `:timestamp` in `embed`.

  ## Examples

  ```elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_timestamp(embed, "2018-04-21T17:33:51.893000Z")
  %Nostrum.Struct.Embed{timestamp: "2018-04-21T17:33:51.893000Z"}
  ```
  """
  @spec put_timestamp(t, timestamp) :: t
  def put_timestamp(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | timestamp: value}
  end

  @doc ~S"""
  Puts the given `value` under `:color` in `embed`.

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_color(embed, 431948)
  %Nostrum.Struct.Embed{color: 431948}
  ```
  """
  @spec put_color(t, color) :: t
  def put_color(%__MODULE__{} = embed, value) do
    %__MODULE__{embed | color: value}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Footer` under `:footer` in `embed`.

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_footer(embed, "Discord API", nil)
  %Nostrum.Struct.Embed{
    footer: %Nostrum.Struct.Embed.Footer{
      text: "Discord API",
      icon_url: nil
    }
  }

  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_footer(embed, "nostrum footer", "https://discord.com/assets/53ef346458017da2062aca5c7955946b.svg")
  %Nostrum.Struct.Embed{
    footer: %Nostrum.Struct.Embed.Footer{
      text: "nostrum footer",
      icon_url: "https://discord.com/assets/53ef346458017da2062aca5c7955946b.svg"
    }
  }
  ```
  """
  @spec put_footer(t, Footer.text(), Footer.icon_url()) :: t
  def put_footer(%__MODULE__{} = embed, text, icon_url \\ nil) do
    footer = %Footer{
      text: text,
      icon_url: icon_url
    }

    %__MODULE__{embed | footer: footer}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Image` under `:image` in `embed`.

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_image(embed, "https://discord.com/assets/af92e60c16b7019f34a467383b31490a.svg")
  %Nostrum.Struct.Embed{
    image: %Nostrum.Struct.Embed.Image{
      url: "https://discord.com/assets/af92e60c16b7019f34a467383b31490a.svg"
    }
  }
  ```
  """
  @spec put_image(t, Image.url()) :: t
  def put_image(%__MODULE__{} = embed, nil) do
    %__MODULE__{embed | image: nil}
  end

  def put_image(%__MODULE__{} = embed, url) do
    image = %Image{
      url: url
    }

    %__MODULE__{embed | image: image}
  end

  @doc ~S"""
  Puts a `Nostrum.Struct.Embed.Thumbnail` under `:thumbnail` in `embed`.

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_thumbnail(embed, "https://discord.com/assets/af92e60c16b7019f34a467383b31490a.svg")
  %Nostrum.Struct.Embed{
    thumbnail: %Nostrum.Struct.Embed.Thumbnail{
      url: "https://discord.com/assets/af92e60c16b7019f34a467383b31490a.svg"
    }
  }
  ```
  """
  @spec put_thumbnail(t, Thumbnail.url()) :: t
  def put_thumbnail(%__MODULE__{} = embed, nil) do
    %__MODULE__{embed | thumbnail: nil}
  end

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

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_author(embed, "skippi", "https://github.com/skippi", nil)
  %Nostrum.Struct.Embed{
    author: %Nostrum.Struct.Embed.Author{
      name: "skippi",
      url: "https://github.com/skippi",
      icon_url: nil
    }
  }
  ```
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

  ## Examples

  ```Elixir
  iex> embed = %Nostrum.Struct.Embed{}
  ...> Nostrum.Struct.Embed.put_field(embed, "First User", "b1nzy")
  %Nostrum.Struct.Embed{
    fields: [
      %Nostrum.Struct.Embed.Field{name: "First User", value: "b1nzy"}
    ]
  }

  iex> embed = %Nostrum.Struct.Embed{
  ...>   fields: [
  ...>     %Nostrum.Struct.Embed.Field{name: "First User", value: "b1nzy"}
  ...>   ]
  ...> }
  ...> Nostrum.Struct.Embed.put_field(embed, "Second User", "Danny")
  %Nostrum.Struct.Embed{
    fields: [
      %Nostrum.Struct.Embed.Field{name: "First User", value: "b1nzy"},
      %Nostrum.Struct.Embed.Field{name: "Second User", value: "Danny"}
    ]
  }
  ```
  """
  @spec put_field(t, Field.name(), Field.value(), Field.inline()) :: t
  def put_field(embed, name, value, inline \\ nil)

  def put_field(%__MODULE__{fields: fields} = embed, name, value, inline) when is_list(fields) do
    field = %Field{
      name: name,
      value: value,
      inline: inline
    }

    %__MODULE__{embed | fields: fields ++ [field]}
  end

  def put_field(embed, name, value, inline) do
    put_field(%__MODULE__{embed | fields: []}, name, value, inline)
  end

  @doc """
  Create an embed from a struct that implements the `Nostrum.Struct.Embed` behaviour
  """
  def from(%module{} = struct) do
    # checks if the struct implements the behaviour
    unless Enum.member?(module.module_info(:attributes), {:behaviour, [__MODULE__]}) do
      raise "#{module} does not implement the behaviour #{__MODULE__}"
    end

    embed =
      %__MODULE__{}
      |> put_color(module.color(struct))
      |> put_description(module.description(struct))
      |> put_image(module.image(struct))
      |> put_thumbnail(module.thumbnail(struct))
      |> put_timestamp(module.timestamp(struct))
      |> put_title(module.title(struct))
      |> put_url(module.url(struct))

    embed =
      case module.author(struct) do
        %Author{} = author -> put_author(embed, author.name, author.url, author.icon_url)
        nil -> embed
        other -> raise "\"#{inspect(other)}\" is invalid for type author()"
      end

    embed =
      case module.footer(struct) do
        %Footer{} = footer -> put_footer(embed, footer.text, footer.icon_url)
        nil -> embed
        other -> raise "\"#{inspect(other)}\" is invalid for type footer()"
      end

    struct
    |> module.fields()
    |> List.wrap()
    |> Enum.reduce(embed, fn
      %Field{} = field, embed -> put_field(embed, field.name, field.value, field.inline)
      other, _ -> raise "\"#{inspect(other)}\" is invalid for type fields()"
    end)
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
