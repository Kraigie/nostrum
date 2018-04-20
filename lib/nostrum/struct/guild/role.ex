defmodule Nostrum.Struct.Guild.Role do
  @moduledoc ~S"""
  Struct representing a Discord role.

  ## Using Roles in Messages

  A `Nostrum.Struct.Guild.Role` can be used in message content using the `String.Chars`
  protocol.

  ```Elixir
  role = %Nostrum.Struct.Role{id: 43819043108}

  Nostrum.Api.create_message!(189098431762321, "#{role}")
  ```
  """

  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util

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

  defimpl String.Chars do
    def to_string(role), do: "<@&#{role.id}>"
  end

  @typedoc "The id of the role"
  @type id :: Snowflake.t()

  @typedoc "The name of the role"
  @type name :: String.t()

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

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
