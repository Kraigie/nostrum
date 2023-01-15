defmodule Nostrum.Struct.Message.Sticker do
  @moduledoc """
  A `Nostrum.Struct.Message.Sticker` represents a sticker that can be sent inside a `Nostrum.Struct.Message`.

  More information can be found on the [Discord API Sticker Object Documentation.](https://discord.com/developers/docs/resources/sticker#sticker-object-sticker-structure)
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.{Snowflake, Util}

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
  Id of the sticker
  """
  @type id :: Snowflake.t()

  @typedoc """
  Id of the pack the sticker is from
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
  Discord name of a unicode emoji representing the sticker's expression. for standard stickers, a comma-separated list of related expressions.
  """
  @type tags :: String.t() | nil

  @typedoc """
  [Discord API Sticker Object Type Documentation](https://discord.com/developers/docs/resources/sticker#sticker-object-sticker-types)

  - `1`  - `STANDARD`       an official sticker in a pack, part of Nitro or in a removed purchasable pack
  - `2`  - `GUILD`          a sticker uploaded to a Boosted guild for the guild's members
  """
  @type type :: integer()

  @typedoc """
  [Discord API Sticker Object Format Type Documentation](https://discord.com/developers/docs/resources/sticker#sticker-object-sticker-format-types)

  - `1`  - `PNG`
  - `2`  - `APNG`
  - `3`  - `LOTTIE`
  """
  @type format_type :: integer()

  @typedoc """
  Whether this guild sticker can be used, may be false due to loss of Server Boosts
  """
  @type available :: boolean

  @typedoc """
  Id of the guild that owns this sticker
  """
  @type guild_id :: Guild.id() | nil

  @typedoc """
  User that uploaded the guild sticker
  """
  @type user :: User.t()

  @typedoc """
  The sticker's sort order within its pack
  """
  @type sort_value :: integer()

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

    struct(__MODULE__, new)
  end
end
