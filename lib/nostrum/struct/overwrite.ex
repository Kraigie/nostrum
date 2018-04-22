defmodule Nostrum.Struct.Overwrite do
  @moduledoc """
  Struct representing a Discord overwrite.
  """

  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Snowflake
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [
    :id,
    :name,
    :allow,
    :deny
  ]

  @typedoc "Role or User id"
  @type id :: Role.id() | User.id()

  @typedoc "Either 'role' or 'member'"
  @type name :: String.t()

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
