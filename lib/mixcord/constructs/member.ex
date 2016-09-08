defmodule Mixcord.Constructs.Member do
  @moduledoc """
  Struct representing a Discord guild member and various helper functions.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
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