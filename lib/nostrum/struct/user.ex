defmodule Nostrum.Struct.User do
  @moduledoc ~S"""
  Struct representing a Discord user.

  ## Using Users in Messages

  A `Nostrum.Struct.User` can be used in message content using the `String.Chars`
  protocol or `mention/1`.

  ```Elixir
  user = %Nostrum.Struct.User{id: 120571255635181568}
  Nostrum.Api.create_message!(184046599834435585, "#{user}")
  %Nostrum.Struct.Message{}

  user = %Nostrum.Struct.User{id: 89918932789497856}
  Nostrum.Api.create_message!(280085880452939778, "#{Nostrum.Struct.User.mention(user)}")
  %Nostrum.Struct.Message{}
  ```

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util

  defstruct [
    :id,
    :username,
    :discriminator,
    :avatar,
    :bot,
    :mfa_enabled,
    :verified,
    :email
  ]

  defimpl String.Chars do
    def to_string(user), do: @for.mention(user)
  end

  @typedoc "The user's id"
  @type id :: Snowflake.t()

  @typedoc "The user's username"
  @type username :: String.t()

  @typedoc "The user's 4--digit discord-tag"
  @type discriminator :: String.t()

  @typedoc "User's avatar hash"
  @type avatar :: String.t() | nil

  @typedoc "Whether the user is a bot"
  @type bot :: boolean | nil

  @typedoc "Whether the user has two factor enabled"
  @type mfa_enabled :: boolean | nil

  @typedoc "Whether the email on the account has been verified"
  @type verified :: boolean | nil

  @typedoc "The user's email"
  @type email :: String.t() | nil

  @type t :: %__MODULE__{
          id: id,
          username: username,
          discriminator: discriminator,
          avatar: avatar,
          bot: bot,
          mfa_enabled: mfa_enabled,
          verified: verified,
          email: email
        }

  @doc ~S"""
  Formats an `Nostrum.Struct.User` into a mention.

  ## Examples

  ```Elixir
  iex> user = %Nostrum.Struct.User{id: 177888205536886784}
  ...> Nostrum.Struct.User.mention(user)
  "<@177888205536886784>"
  ```
  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{id: id}), do: "<@#{id}>"

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
