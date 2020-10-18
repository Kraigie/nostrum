defmodule Nostrum.Struct.Event.InviteCreate do
  @moduledoc """
  Struct representing an Invite Create event
  """

  alias Nostrum.Struct.{Channel, Guild, User}
  alias Nostrum.Util

  defstruct [
    :channel_id,
    :code,
    :created_at,
    :guild_id,
    :inviter,
    :max_age,
    :max_uses,
    :target_user,
    :target_user_type,
    :temporary,
    :uses
  ]

  @typedoc """
  Channel id of the channel this invite is for.
  """
  @type channel_id :: Channel.id()

  @typedoc """
  The unique invite code.
  """
  @type code :: String.t()

  @typedoc """
  The time at which the invite was created.
  """
  @type created_at :: String.t()

  @typedoc """
  Guild id of the guild this invite is for.
  """
  @type guild_id :: Guild.id() | nil

  @typedoc """
  The user that created the invite.
  """
  @type inviter :: User.t() | nil

  @typedoc """
  Duration (in seconds) after which the invite expires.
  """
  @type max_age :: integer

  @typedoc """
  Max number of times this invite can be used.
  """
  @type max_uses :: integer

  @typedoc """
  Partially populated user struct of the target user for this invite.
  """
  @type target_user :: User.t() | nil

  @typedoc """
  The type of user target for this invite.
  """
  @type target_user_type :: integer | nil

  @typedoc """
  Whether this invite only grants temporary membership.
  """
  @type temporary :: boolean

  @typedoc """
  Number of times this invite has been used.
  """
  @type uses :: integer

  @type t :: %__MODULE__{
          channel_id: channel_id,
          code: code,
          created_at: created_at,
          guild_id: guild_id,
          inviter: inviter,
          max_age: max_age,
          max_uses: max_uses,
          target_user: target_user,
          target_user_type: target_user_type,
          temporary: temporary,
          uses: uses
        }

  @doc false
  def to_struct(map) do
    atom_map = map |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    __MODULE__
    |> struct(atom_map)
    |> Map.update(:inviter, nil, &Util.cast(&1, {:struct, User}))
    |> Map.update(:target_user, nil, &Util.cast(&1, {:struct, User}))
  end
end
