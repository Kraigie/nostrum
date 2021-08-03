defmodule Nostrum.Struct.Message.Sticker do
  @moduledoc """
  Struct representing a Discord message.
  """

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
  	id of the sticker
  """
  @type id :: Snowflake.t()

  @typedoc """
  for standard stickers, id of the pack the sticker is from
  """
  @type pack_id :: Snowflake.t()

  @typedoc """
  name of the sticker
  """
  @type name :: String.t() | nil

  @typedoc """
  	description of the sticker
  """
  @type description :: String.t() | nil

  @typedoc """
  for guild stickers, the Discord name of a unicode emoji representing the sticker's expression. for standard stickers, a comma-separated list of related expressions.
  """
  @type tags :: String.t() | nil

  @typedoc """
  Sticker Types:

  TYPE     |	VALUE 	|DESCRIPTION
  ---------|----------|-----------------------------------------------------------------------------
  STANDARD |	1     	|an official sticker in a pack, part of Nitro or in a removed purchasable pack
  GUILD    |	2     	|a sticker uploaded to a Boosted guild for the guild's members
  """
  @type type :: integer

  @typedoc """
  Sticker Format Types

  TYPE        | VALUE
  ------------| ---------
  PNG         | 1
  APNG        | 2
  LOTTIE      | 3
  """
  @type format_type :: integer

  @typedoc """
  whether this guild sticker can be used, may be false due to loss of Server Boosts
  """
  @type available :: boolean

  @typedoc """
  id of the guild that owns this sticker
  """
  @type guild_id :: Guild.id() | nil

  @typedoc """
  the user that uploaded the guild sticker
  """
  @type user :: User.t()

  @typedoc """
  the standard sticker's sort order within its pack
  """
  @type sort_value :: integer

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
  def p_encode do
    %__MODULE__{
      user: User.p_encode()
    }
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:pack_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, Snowflake}))

    struct(__MODULE__, new)
  end
end
