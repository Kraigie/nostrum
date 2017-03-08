defmodule Nostrum.Struct.Invite.Channel do
  @moduledoc """
  Struct representing a Discord invite channel.
  """

  @typedoc "Id of the channel"
  @type id :: integer

  @typedoc "Name of the channel"
  @type name :: String.t

  @typedoc "Type of channel"
  @type type :: String.t

  @type t :: %__MODULE__{
    id: id,
    name: name,
    type: type
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name,
    :type
  ]

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
