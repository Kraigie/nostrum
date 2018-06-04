defmodule Nostrum.Struct.Invite.Metadata do
  @moduledoc ~S"""
  Struct representing Discord invite metadata.
  """

  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [
    :inviter,
    :uses,
    :max_uses,
    :max_age,
    :temporary,
    :created_at,
    :revoked
  ]

  @typedoc """
  User who created the invite.
  """
  @type inviter :: User.t()

  @typedoc """
  Number of times this invite has been used.
  """
  @type uses :: integer

  @typedoc """
  Max number of times this invite can be used.
  """
  @type max_uses :: integer

  @typedoc """
  Duration (in seconds) after which the invite expires.
  """
  @type max_age :: integer

  @typedoc """
  Whether this invite only grants temporary membership.
  """
  @type temporary :: boolean

  @typedoc """
  When this invite was created.
  """
  @type created_at :: String.t()

  @typedoc """
  Whether this invite is revoked.
  """
  @type revoked :: boolean

  @type t :: %__MODULE__{
          inviter: inviter,
          uses: uses,
          max_uses: max_uses,
          max_age: max_age,
          temporary: temporary,
          created_at: created_at,
          revoked: revoked
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:inviter, nil, &Util.cast(&1, {:struct, User}))

    struct(__MODULE__, new)
  end
end
