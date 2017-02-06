defmodule Mixcord.Struct.Role do
  @moduledoc """
  Struct representing a Discord role.
  """

  @typedoc "The id of the role"
  @type id :: integer

  @typedoc "The name of the role"
  @type name :: String.t

  @typedoc "The hexadecimal color code"
  @type color :: integer

  @typedoc "Whether the role is pinned in the user listing"
  @type hoist :: boolean

  @typedoc "The position of the role"
  @type position :: integer

  @typedoc "The permission bit set"
  @type permissions :: integer

  @typedoc "Whether the role is managed by an integration"
  @type managed :: boolean

  @typedoc "Whether the role is mentionable"
  @type mentionable :: boolean

  @type t :: %__MODULE__{
    id: id,
    name: name,
    color: color,
    hoist: hoist,
    position: position,
    permissions: permissions,
    managed: managed,
    mentionable: mentionable
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name,
    :color,
    :hoist,
    :position,
    :permissions,
    :managed,
    :mentionable
  ]
end
