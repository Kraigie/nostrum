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
    :target_user,
    :target_user_type,
    :approximate_presence_count,
    :approximate_member_count,
    :uses,
    :max_uses,
    :max_age,
    :temporary,
    :created_at
  ]

  @typedoc """
  The invite code (unique ID).
  """
  @type code :: String.t()

  @typedoc """
  Partially populated guild struct of the guild this invite is for.
  """
  @type guild :: Guild.t() | nil

  @typedoc """
  The channel this invite is for.
  """
  @type channel :: Channel.t()

  @typedoc """
  The user who created this invite.
  """
  @type inviter :: User.t() | nil

  @typedoc """
  Partially populated user struct of the target user for this invite.
  """
  @type target_user :: User.t() | nil

  @typedoc """
  The type of user target for this invite.
  """
  @type target_user_type :: integer | nil

  @typedoc """
  Approximate online member count of the guild this invite is for.
  """
  @type approximate_presence_count :: integer | nil

  @typedoc """
  Approximate total member count of the guild this invite is for.
  """
  @type approximate_member_count :: integer | nil

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
  An invite without metadata.
  """
  @type simple_invite :: %__MODULE__{
          code: code,
          guild: guild,
          channel: channel,
          inviter: inviter,
          target_user: target_user,
          target_user_type: target_user_type,
          approximate_presence_count: approximate_presence_count,
          approximate_member_count: approximate_member_count,
          uses: nil,
          max_uses: nil,
          max_age: nil,
          temporary: nil,
          created_at: nil
        }

  @typedoc """
  An invite with metadata.
  """
  @type detailed_invite :: %__MODULE__{
          code: code,
          guild: guild,
          channel: channel,
          inviter: inviter,
          target_user: target_user,
          target_user_type: target_user_type,
          approximate_presence_count: approximate_presence_count,
          approximate_member_count: approximate_member_count,
          uses: uses,
          max_uses: max_uses,
          max_age: max_age,
          temporary: temporary,
          created_at: created_at
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
    |> Map.update(:target_user, nil, &Util.cast(&1, {:struct, User}))
  end
end
