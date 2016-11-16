defmodule Mixcord.Map.Member do
  @moduledoc """
  Struct representing a Discord guild member.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  alias Mixcord.Map.{Role, User}

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

  @type t :: Map.t

  @doc """
  Represents a Discord Member.

  * `:guild_id` - *String*. Id of the guild the member is a part of.
  * `:user` - *Struct*. A `Mixcord.Map.User` struct.
  * `:nick` - *?String*. Users guild nickname (if one is set).
  * `:roles` - *List*. A list of `Mixcord.Map.Role` ids.
  * `:joined_at` - *Data*. Date the user joined the guild.
  * `:deaf` - *Boolean*. If the user is deafened.
  * `:mute` - *Boolean*. If the user is muted.
  """
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
end
