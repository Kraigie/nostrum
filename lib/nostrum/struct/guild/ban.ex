defmodule Nostrum.Struct.Guild.Ban do
  @moduledoc """
  Represents a guild ban.
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [
    :reason,
    :user
  ]

  @typedoc "The reason for the ban"
  @type reason :: String.t() | nil

  @typedoc "The banned user"
  @type user :: User.t()

  @type t :: %__MODULE__{
          reason: reason,
          user: user
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))

    struct(__MODULE__, new)
  end
end
