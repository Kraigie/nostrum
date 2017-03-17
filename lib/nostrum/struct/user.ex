defmodule Nostrum.Struct.User do
  @moduledoc """
  Struct representing a Discord user.

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  @typedoc "The user's id"
  @type id :: integer

  @typedoc "The user's username"
  @type username :: String.t

  @typedoc "The user's 4--digit discord-tag"
  @type discriminator :: String.t

  @typedoc "User's avatar hash"
  @type avatar :: String.t

  @typedoc "Whether the user is a bot"
  @type bot :: boolean

  @typedoc "Whether the user has two factor enabled"
  @type mfa_enabled :: boolean

  @typedoc "Whether the email on the account has been verified"
  @type verified :: boolean

  @typedoc "The user's email"
  @type email :: String.t

  @type t :: %__MODULE__{
    id: id,
    username: username,
    discriminator: discriminator,
    avatar: avatar,
    bot: bot,
    mfa_enabled: mfa_enabled,
    verified: verified,
    email: email,
  }

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

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  @doc false
  def to_struct(%{__struct__: _} = map), do: map
  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
