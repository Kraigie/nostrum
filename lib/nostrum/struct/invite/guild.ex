defmodule Nostrum.Struct.Invite.Guild do
  @moduledoc """
  Struct representing a Discord invite guild.
  """

  @typedoc "Id of the guild"
  @type id :: integer

  @typedoc "Name of the guild"
  @type name :: String.t

  @typedoc "Hash of the guild splash"
  @type splash :: String.t

  @typedoc "Hash of the guild icon"
  @type icon :: String.t

  @type t :: %__MODULE__{
    id: id,
    name: name,
    splash: splash,
    icon: icon
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name,
    :splash,
    :icon
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
