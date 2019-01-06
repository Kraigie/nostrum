defmodule Nostrum.Struct.Invite do
  @moduledoc """
  Struct representing a Discord invite.
  """

  alias Nostrum.Struct.{Channel, Guild, User}
  alias Nostrum.Util

  defstruct [
    :code,
    :guild,
    :channel,
    :inviter,
    :uses,
    :max_uses,
    :max_age,
    :temporary,
    :created_at,
    :revoked
  ]

  @typedoc """
  The invite code (unique ID).
  """
  @type code :: String.t()

  @typedoc """
  The guild this invite is for.
  """
  @type guild :: Guild.t()

  @typedoc """
  The channel this invite is for.
  """
  @type channel :: Channel.t()

  @typedoc """
  The user who created this invite.
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

  @typedoc """
  An invite without metadata.
  """
  @type simple_invite :: %__MODULE__{
          code: code,
          guild: guild,
          channel: channel,
          inviter: inviter,
          uses: nil,
          max_uses: nil,
          max_age: nil,
          temporary: nil,
          created_at: nil,
          revoked: nil
        }

  @typedoc """
  An invite with metadata.
  """
  @type detailed_invite :: %__MODULE__{
          code: code,
          guild: guild,
          channel: channel,
          inviter: inviter,
          uses: uses,
          max_uses: max_uses,
          max_age: max_age,
          temporary: temporary,
          created_at: created_at,
          revoked: revoked
        }

  @type t :: simple_invite | detailed_invite

  @doc false
  def to_struct(map) do
    atom_map = map |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    __MODULE__
    |> struct(atom_map)
    |> Map.update(:guild, nil, &Util.cast(&1, {:struct, Guild}))
    |> Map.update(:channel, nil, &Util.cast(&1, {:struct, Channel}))
    |> Map.update(:inviter, nil, &Util.cast(&1, {:struct, User}))
  end
end
