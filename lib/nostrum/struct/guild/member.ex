defmodule Nostrum.Struct.Guild.Member do
  @moduledoc """
  Struct representing a Discord guild member.

  A `Nostrum.Struct.Guild.Member` is represented internally by a `%Nostrum.Struct.Guild.Member{}`
  struct. It stores information regarding a `Nostrum.Struct.User`'s properties pertaining to a
  specific `Nostrum.Struct.Guild`.

  Keep in mind that a `Nostrum.Struct.Guild.Member` has no knowledge of its respective
  `Nostrum.Struct.Guild`. Thus, it is the lib user's responsibility to ensure that its
  relationship is not lost.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  alias Nostrum.Struct.Snowflake
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [
    :user,
    :nick,
    :roles,
    :joined_at,
    :deaf,
    :mute
  ]

  @typedoc "The user struct"
  @type user :: User.t()

  @typedoc "The nickname of the user"
  @type nick :: String.t() | nil

  @typedoc "A list of role ids"
  @type roles :: [Snowflake.t()]

  @typedoc "Date the user joined the guild"
  @type joined_at :: String.t()

  @typedoc "Whether the user is deafened"
  @type deaf :: boolean

  @typedoc "Whether the user is muted"
  @type mute :: boolean

  @type t :: %__MODULE__{
          user: user,
          nick: nick,
          roles: roles,
          joined_at: joined_at,
          deaf: deaf,
          mute: mute
        }

  @doc false
  def p_encode do
    %__MODULE__{
      user: User.p_encode()
    }
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))
      |> Map.update(:roles, nil, &Util.cast(&1, {:list, Snowflake}))

    struct(__MODULE__, new)
  end
end
