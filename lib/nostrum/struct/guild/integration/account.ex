defmodule Nostrum.Struct.Guild.Integration.Account do
  @moduledoc """
  Struct representing a Discord guild's integration account.
  """
  @moduledoc since: "0.5.1"

  alias Nostrum.Util

  defstruct [:id, :name]

  @typedoc """
  The id of the account
  """
  @type id :: String.t()

  @typedoc """
  The name of the account
  """
  @type name :: String.t()

  @type t :: %__MODULE__{
          id: id,
          name: name
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
