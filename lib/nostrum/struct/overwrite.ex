defmodule Nostrum.Struct.Overwrite do
  @moduledoc """
  Struct representing a Discord overwrite.
  """

  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util

  defstruct [
    :id,
    :name,
    :allow,
    :deny
  ]

  @typedoc "Role or User id"
  @type id :: Snowflake.t

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

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
    |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
  end
end
