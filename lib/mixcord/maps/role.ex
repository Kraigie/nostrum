defmodule Mixcord.Map.Role do
  @moduledoc """
  Struct representing a Discord role.
  """

  @Type id :: integer
  @Type name :: String.t
  @Type color :: integer
  @Type hoist :: boolean
  @Type position :: integer
  @Type permissions :: integer
  @Type managed :: boolean
  @Type mentionable :: boolean

  @Type t :: Map.t

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
