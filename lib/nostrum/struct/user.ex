defmodule Nostrum.Struct.User do
  @moduledoc ~S"""
  Struct representing a Discord user.

  ## Mentioning Users in Messages

  A `Nostrum.Struct.User` can be mentioned in message content using the `String.Chars`
  protocol or `mention/1`.

  ```elixir
  user = %Nostrum.Struct.User{id: 120571255635181568}
  Nostrum.Api.create_message!(184046599834435585, "#{user}")
  %Nostrum.Struct.Message{content: "<@120571255635181568>"}

  user = %Nostrum.Struct.User{id: 89918932789497856}
  Nostrum.Api.create_message!(280085880452939778, "#{Nostrum.Struct.User.mention(user)}")
  %Nostrum.Struct.Message{content: "<@89918932789497856>"}
  ```

  ## User vs. Member
  A `user` contains only general information about that user such as a `username` and an `avatar`.
  A `member` has everything that a `user` has, but also additional information on a per guild basis. This includes things like a `nickname` and a list of `roles`.
  """

  alias Nostrum.{Constants, Snowflake, Struct.User.Flags, Util}

  defstruct [
    :id,
    :username,
    :discriminator,
    :avatar,
    :bot,
    :mfa_enabled,
    :verified,
    :email,
    :public_flags
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

  @typedoc "The user's public flags"
  @type public_flags :: Flags.t()

  @type t :: %__MODULE__{
          id: id,
          username: username,
          discriminator: discriminator,
          avatar: avatar,
          bot: bot,
          mfa_enabled: mfa_enabled,
          verified: verified,
          email: email,
          public_flags: public_flags
        }

  @doc ~S"""
  Formats an `Nostrum.Struct.User` into a mention.

  ## Examples

  ```elixir
  iex> user = %Nostrum.Struct.User{id: 177888205536886784}
  ...> Nostrum.Struct.User.mention(user)
  "<@177888205536886784>"
  ```
  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{id: id}), do: "<@#{id}>"

  @doc """
  Returns the URL of a user's display avatar.

  If `:avatar` is `nil`, the default avatar url is returned.

  Supported image formats are PNG, JPEG, WebP, and GIF.

  ## Examples

  ```elixir
  iex> user = %Nostrum.Struct.User{avatar: "8342729096ea3675442027381ff50dfe",
  ...>                             id: 80351110224678912}
  iex> Nostrum.Struct.User.avatar_url(user)
  "https://cdn.discordapp.com/avatars/80351110224678912/8342729096ea3675442027381ff50dfe.webp"
  iex> Nostrum.Struct.User.avatar_url(user, "png")
  "https://cdn.discordapp.com/avatars/80351110224678912/8342729096ea3675442027381ff50dfe.png"

  iex> user = %Nostrum.Struct.User{avatar: nil,
  ...>                             discriminator: "1337"}
  iex> Nostrum.Struct.User.avatar_url(user)
  "https://cdn.discordapp.com/embed/avatars/2.png"
  ```
  """
  @spec avatar_url(t, String.t()) :: String.t()
  def avatar_url(user, image_format \\ "webp")

  def avatar_url(%__MODULE__{avatar: nil, discriminator: disc}, _) do
    image_name =
      disc
      |> String.to_integer()
      |> rem(5)

    URI.encode(Constants.cdn_url() <> Constants.cdn_embed_avatar(image_name))
  end

  def avatar_url(%__MODULE__{id: id, avatar: avatar}, image_format),
    do: URI.encode(Constants.cdn_url() <> Constants.cdn_avatar(id, avatar, image_format))

  @doc """
  Returns a user's `:username` and `:discriminator` separated by a hashtag.

  ## Examples

  ```elixir
  iex> user = %Nostrum.Struct.User{username: "b1nzy",
  ...>                             discriminator: "0852"}
  iex> Nostrum.Struct.User.full_name(user)
  "b1nzy#0852"
  ```
  """
  @spec full_name(t) :: String.t()
  def full_name(%__MODULE__{username: username, discriminator: disc}),
    do: "#{username}##{disc}"

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
      |> Map.update(:public_flags, %Flags{}, &Flags.from_integer(&1))

    struct(__MODULE__, new)
  end
end
