defmodule Mixcord.Struct.Overwrite do
  @moduledoc """
  Struct representing a Discord overwrite.
  """

  @typedoc "Role or User id"
  @type id :: integer

  @typedoc "Either 'role' or 'member'"
  @type name :: String.t

  @typedoc "Permission bit set"
  @type allow :: integer

  @typedoc "Permission but set"
  @type deny :: integer

  @type t :: %__MODULE__{
    id: id,
    name: name,
    allow: allow,
    deny: deny
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name,
    :allow,
    :deny
  ]
end
