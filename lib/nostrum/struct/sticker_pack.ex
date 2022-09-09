defmodule Nostrum.Struct.StickerPack do
  @moduledoc """
  Represents a sticker pack
  """

  alias Nostrum.Snowflake
  alias Nostrum.Struct.Message.Sticker
  alias Nostrum.Util

  defstruct [
    :id,
    :stickers,
    :name,
    :sku_id,
    :cover_sticker_id,
    :description,
    :banner_asset_id
  ]

  @typedoc "ID of the sticker pack"
  @type id :: Snowflake.t()

  @typedoc "A list of stickers in the pack"
  @type stickers :: [Sticker]

  @typedoc "Name of the sticker pack"
  @type name :: String.t()

  @typedoc "ID of the pack's SKU"
  @type sku_id :: Snowflake.t()

  @typedoc "ID of the stick that is shown as the pack's icon"
  @type cover_sticker_id :: Snowflake.t() | nil

  @typedoc "Description of the sticker pack"
  @type description :: String.t()

  @typedoc "ID of the sticker pack's banner image"
  @type banner_asset_id :: Snowflake.t() | nil

  @type t :: %__MODULE__{
    id: id,
    stickers: stickers,
    name: name,
    sku_id: sku_id,
    cover_sticker_id: cover_sticker_id,
    description: description,
    banner_asset_id: banner_asset_id
  }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:stickers, nil, &Util.cast(&1, {:list, {:struct, Sticker}}))
      |> Map.update(:sku_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:cover_sticker_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:banner_asset_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end

end
