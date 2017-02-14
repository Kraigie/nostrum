defmodule Mixcord.Struct.Member do
  @moduledoc """
  Struct representing a Discord guild member.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  alias Mixcord.Struct.{Role, User}
  alias Mixcord.Util

  @typedoc "Id of the guild the member is part of"
  @type guild_id :: integer

  @typedoc "The user struct"
  @type user :: User.t

  @typedoc "The nickname of the user"
  @type nick :: String.t | nil

  @typedoc "A list of role ids"
  @type roles :: list(Role.t)

  @typedoc "Date the user joined the guild"
  @type joined_at :: String.t

  @typedoc "Whether the user is deafened"
  @type deaf :: boolean

  @typedoc "Whether the user is muted"
  @type mute :: boolean

  @type t :: %__MODULE__{
    guild_id: guild_id,
    user: user,
    nick: nick,
    roles: roles,
    joined_at: joined_at,
    deaf: deaf,
    mute: mute
  }

  @derive [Poison.Encoder]
  defstruct [
    :guild_id,
    :user,
    :nick,
    :roles,
    :joined_at,
    :deaf,
    :mute
  ]

  @doc false
  def to_struct(map) do
    new = map
    |> Map.update(:user, %{}, &User.to_struct(&1))
    |> Map.update(:roles, %{}, &Util.list_to_struct_list(&1, Role))
    struct(__MODULE__, new)
  end
end
