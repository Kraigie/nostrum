defmodule Mixcord.Map.Role do
  @moduledoc """
  Struct representing a Discord role.
  """

  @type id :: integer
  @type name :: String.t
  @type color :: integer
  @type hoist :: boolean
  @type position :: integer
  @type permissions :: integer
  @type managed :: boolean
  @type mentionable :: boolean

  @type t :: Map.t

  @doc """
  Represents a Discord Role.

  * `:id` - *Integer*. Id of the role.
  * `:name` - *String*. Name of the role.
  * `:color` - *Integer*. Integer representation of hexadecimal color code.
  * `:hoist` - *Boolean*. If the role is pinned in the user listing.
  * `:position` - *Integer*. Position of the role.
  * `:permissions` - *Integer*. Permission bit set.
  * `:managed` - *Boolean*. Whether this role is managed by an integration.
  * `:mentionable` - *Boolean*. Whether this role is mentionable.
  """
  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name,
    :color,
    :hoist,
    :position,
    :permissions,
    :managed,
    :mentionable,
  ]
end
