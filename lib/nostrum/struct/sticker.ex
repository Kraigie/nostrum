defmodule Nostrum.Struct.Sticker do
  @moduledoc """
  A `Nostrum.Struct.Sticker` represents a sticker that can be sent inside a
  `Nostrum.Struct.Message`.
  """
  @moduledoc since: "0.10.0"

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.{Constants, Snowflake, Util}

  defstruct [
    :id,
    :pack_id,
    :name,
    :description,
    :tags,
    :type,
    :format_type,
    :available,
    :guild_id,
    :user,
    :sort_value
  ]

  @typedoc """
  ID of the sticker
  """
  @type id :: Snowflake.t()

  @typedoc """
  ID of the pack the sticker is from
  """
  @type pack_id :: Snowflake.t()

  @typedoc """
  Name of the sticker
  """
  @type name :: String.t() | nil

  @typedoc """
  Description of the sticker
  """
  @type description :: String.t() | nil

  @typedoc """
  Tags used by the Discord client to auto-complete a sticker.

  For default sticker packs, this is a comma-separated list. For guild stickers,
  this is the name of the unicode emoji associated by the sticker creator with
  the sticker.

  This is technically a free-text field so consistency in formatting is not guaranteed.
  """
  @type tags :: String.t()

  @typedoc """
  Whether the sticker is a standard (platform made) sticker or a custom guild sticker.
  """
  @type type :: :standard | :guild

  @typedoc """
  Format of the sticker.

  This field is used to determine the return URL in `cdn_url/1`.
  """
  @type format_type :: :png | :apng | :lottie | :gif

  @typedoc """
  Whether this guild sticker can be used.

  May be false due to loss of Server Boosts
  """
  @type available :: boolean

  @typedoc """
  ID of the guild that owns this sticker.

  `nil` if the sticker is a built-in (type `:standard`) sticker.
  """
  @type guild_id :: Guild.id() | nil

  @typedoc """
  User that uploaded the guild sticker.

  `nil` if the sticker is a built-in (type `:standard`) sticker.
  """
  @type user :: User.t() | nil

  @typedoc """
  The sticker's sort order within its pack.

  Sometimes provided for stickers with type `:standard` that are in a pack.
  """
  @type sort_value :: integer() | nil

  @type t :: %__MODULE__{
          id: id,
          pack_id: pack_id,
          name: name,
          description: description,
          tags: tags,
          type: type,
          format_type: format_type,
          available: available,
          guild_id: guild_id,
          user: user,
          sort_value: sort_value
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:pack_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))
      |> Map.update(:format_type, nil, &cast_format_type/1)
      |> Map.update(:type, nil, &cast_type/1)

    struct(__MODULE__, new)
  end

  @doc false
  defp cast_format_type(format_type) do
    case format_type do
      1 -> :png
      2 -> :apng
      3 -> :lottie
      4 -> :gif
    end
  end

  @doc false
  defp cast_type(type) do
    case type do
      1 -> :standard
      2 -> :guild
    end
  end

  @doc ~S"""
  Fetch a CDN URL for the sticker object.

  `:png` and `:apng` stickers will return a `.png` URL, `:gif` will return a
  `.gif` URL and `:lottie` will return a `.json` URL.

  ### Examples

  ```elixir
  iex> sticker = %Nostrum.Struct.Sticker{format_type: :gif, id: 112233445566778899}
  iex> Nostrum.Struct.Sticker.cdn_url sticker
  "https://media.discordapp.net/stickers/112233445566778899.gif"
  ```

  ```elixir
  iex> sticker = %Nostrum.Struct.Sticker{format_type: :apng, id: 998877665544332211}
  iex> Nostrum.Struct.Sticker.cdn_url sticker
  "https://cdn.discordapp.com/stickers/998877665544332211.png"
  ```
  """
  @spec cdn_url(t()) :: String.t()
  def cdn_url(%{format_type: :gif, id: id}) do
    Constants.media_url() <> Constants.cdn_sticker(id, "gif")
  end

  def cdn_url(%{format_type: :lottie, id: id}) do
    Constants.cdn_url() <> Constants.cdn_sticker(id, "json")
  end

  def cdn_url(%{format_type: format, id: id}) when format in [:png, :apng] do
    Constants.cdn_url() <> Constants.cdn_sticker(id, "png")
  end
end
