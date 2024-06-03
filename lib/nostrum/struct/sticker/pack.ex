defmodule Nostrum.Struct.Sticker.Pack do
  @moduledoc """
  Represents a platform-curated sticker pack on Discord
  """
  @moduledoc since: "0.10.0"

  defstruct [
    :id,
    :stickers,
    :name,
    :sku_id,
    :cover_sticker_id,
    :description,
    :banner_asset_id
  ]

  alias Nostrum.{Constants, Snowflake, Util}
  alias Nostrum.Struct.Sticker

  @typedoc """
  ID of the sticker pack.
  """
  @type id :: Snowflake.t()

  @typedoc """
  A list of stickers contained within the pack.
  """
  @type stickers :: [Sticker.t()]

  @typedoc """
  Name of the pack.
  """
  @type name :: String.t()

  @typedoc """
  SKU ID of the sticker pack.
  """
  @type sku_id :: Snowflake.t()

  @typedoc """
  ID of a sticker contained within the pack that should be the cover.
  """
  @type cover_sticker_id :: Sticker.id()

  @typedoc """
  Marketing description of the sticker pack.
  """
  @type description :: String.t()

  @typedoc """
  Asset ID of the banner for this sticker pack.
  """
  @type banner_asset_id :: Snowflake.t()

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
      |> Map.update(:stickers, nil, &Util.cast(&1, {:list, {:struct, Sticker}}))
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:sku_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:cover_sticker_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:banner_asset_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end

  @doc ~S"""
  Return the banner pack URL for a given sticker pack.

  This is a marketing banner provided by Discord for their platform curated sticker packs.

  ### Examples

  ```elixir
  iex> pack = %Nostrum.Struct.Sticker.Pack{banner_asset_id: 112233445566778899}
  iex> Nostrum.Struct.Sticker.Pack.banner_url pack
  "https://cdn.discordapp.com/app-assets/710982414301790216/store/112233445566778899.png"
  ```
  """
  @spec banner_url(t()) :: String.t()
  def banner_url(%{banner_asset_id: id}) do
    Constants.cdn_url() <> Constants.cdn_sticker_pack(id)
  end
end
