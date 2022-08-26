defmodule Nostrum.Struct.StickerPacks do
  @moduledoc """
  Represents a sticker pack list
  """

  alias Nostrum.Struct.StickerPack
  alias Nostrum.Util

  defstruct [:sticker_packs]

  @typedoc """
  List of sticker packs
  """
  @type sticker_packs :: [StickerPack]

  @type t :: %__MODULE__{
    sticker_packs: sticker_packs
  }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:sticker_packs, nil, &Util.cast(&1, {:list, {:struct, StickerPack}}))

    struct(__MODULE__, new)
  end
end
