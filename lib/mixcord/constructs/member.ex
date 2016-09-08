defmodule Mixcord.Constructs.Member do
  @moduledoc """
  Struct representing a Discord guild member and various helper functions.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  @doc """
  Defines the Member struct.

  * :user - *Struct*. A `Mixcord.Constructs.User` struct.
  * :nick - *?String*. Users guild nickname (if one is set).
  * :roles - *List*. A list of `Mixcord.Constructs.Role` ids.
  * :joined_at - *Data*. Date the user joined the guild.
  * :deaf - *Boolean*. If the user is deafened.
  * :mute - *Boolean*. If the user is muted.
  """
  @derive [Poison.Encoder]
  defstruct [
    :user,
    :nick,
    :roles,
    :joined_at,
    :deaf,
    :mute
  ]
end