defmodule Mixcord.Map.User do
  @moduledoc """
  Struct representing a Discord user.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

<<<<<<< HEAD
  @type id :: integer
  @type username :: String.t
  @type discriminator :: String.t
  @type avatar :: String.t
  @type bot :: boolean
  @type mfa_enabled :: boolean
  @type verified :: boolean
  @type email :: String.t
=======
  @typedoc "The user's id"
  @Type id :: integer

  @typedoc "The user's username"
  @Type username :: String.t

  @typedoc "The user's 4--digit discord-tag"
  @Type discriminator :: String.t

  @typedoc "User's avatar hash"
  @Type avatar :: String.t

  @typedoc "Whether the user is a bot"
  @Type bot :: boolean

  @typedoc "Whether the user has two factor enabled"
  @Type mfa_enabled :: boolean

  @typedoc "Whether the email on the account has been verified"
  @Type verified :: boolean

  @typedoc "The user's email"
  @Type email :: String.t
>>>>>>> 8766d64815874cdda2fcbd7ac8629f0e078206b8

  @type t :: Map.t

  @doc """
  Represents a Discord User.

  * `:id` - *Integer*. User's id.
  * `:username` - *String*. User's username.
  * `:discriminator` - *String*. User's 4-digit discord-tag.
  * `:avatar` - *String*. User's avatar hash.
  * `:bot` - *Boolean*. Whether the user is a bot.
  * `:mfa_enabled` - *Boolean*. Whether the user has two factor enabled.
  * `:verified` - *Boolean*. Whether the email on the account has been verified.
  * `:email` - *String*. User's email.
  """
  @derive [Poison.Encoder]
  defstruct [
    :id,
    :username,
    :discriminator,
    :avatar,
    :bot,
    :mfa_enabled,
    :verified,
    :email,
  ]
end
