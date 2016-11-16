defmodule Mixcord.Map.Role do
  @moduledoc """
  Struct representing a Discord role.
  """

  @typedoc "The id of the role"
  @Type id :: integer

  @typedoc "The name of the role"
  @Type name :: String.t

  @typedoc "The hexadecimal color code"
  @Type color :: integer

  @typedoc "Whether the role is pinned in the user listing"
  @Type hoist :: boolean

  @typedoc "The position of the role"
  @Type position :: integer

  @typedoc "The permission bit set"
  @Type permissions :: integer

  @typedoc "Whether the role is managed by an integration"
  @Type managed :: boolean

  @typedoc "Whether the role is mentionable"
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
