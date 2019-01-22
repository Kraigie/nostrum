defmodule Nostrum.Struct.Guild.Member do
  @moduledoc ~S"""
  Struct representing a Discord guild member.

  A `Nostrum.Struct.Guild.Member` stores a `Nostrum.Struct.User`'s properties
  pertaining to a specific `Nostrum.Struct.Guild`.

  ## Mentioning Members in Messages

  A `Nostrum.Struct.Guild.Member` can be mentioned in message content using the `String.Chars`
  protocol or `mention/1`.

  ```Elixir
  member = %Nostrum.Struct.Guild.Member{user: Nostrum.Struct.User{id: 120571255635181568}}
  Nostrum.Api.create_message!(184046599834435585, "#{member}")
  %Nostrum.Struct.Message{content: "<@120571255635181568>"}

  member = %Nostrum.Struct.Guild.Member{user: Nostrum.Struct.User{id: 89918932789497856}}
  Nostrum.Api.create_message!(280085880452939778, "#{Nostrum.Struct.Guild.Member.mention(member)}")
  %Nostrum.Struct.Message{content: "<@89918932789497856>"}
  ```
  """

  alias Nostrum.Permission
  alias Nostrum.Struct.{Channel, Guild, Snowflake, User}
  alias Nostrum.Util

  defstruct [
    :user,
    :nick,
    :roles,
    :joined_at,
    :deaf,
    :mute
  ]

  defimpl String.Chars do
    def to_string(member), do: @for.mention(member)
  end

  @typedoc """
  The user struct. This field can be `nil` if Member struct came as a partial Member object included
  in a message received from a guild channel.
  """
  @type user :: User.t() | nil

  @typedoc "The nickname of the user"
  @type nick :: String.t() | nil

  @typedoc "A list of role ids"
  @type roles :: [Snowflake.t()]

  @typedoc """
  Date the user joined the guild.
  If you dont request offline guild members this field will be `nil` for any members that come online.
  """
  @type joined_at :: String.t() | nil

  @typedoc """
  Whether the user is deafened.
  If you dont request offline guild members this field will be `nil` for any members that come online.
  """
  @type deaf :: boolean | nil

  @typedoc """
  Whether the user is muted.
  If you dont request offline guild members this field will be `nil` for any members that come online.
  """
  @type mute :: boolean | nil

  @type t :: %__MODULE__{
          user: user,
          nick: nick,
          roles: roles,
          joined_at: joined_at,
          deaf: deaf,
          mute: mute
        }

  @doc ~S"""
  Formats a `Nostrum.Struct.Guild.Member` into a mention.

  ## Examples

  ```Elixir
  iex> member = %Nostrum.Struct.Guild.Member{user: %Nostrum.Struct.User{id: 177888205536886784}}
  ...> Nostrum.Struct.Guild.Member.mention(member)
  "<@177888205536886784>"
  ```
  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{user: user}), do: User.mention(user)

  @doc """
  Returns a member's guild permissions.

  ## Examples

  ```Elixir
  guild = Nostrum.Cache.GuildCache.get!(279093381723062272)
  member = Map.get(guild.members, 177888205536886784)
  Nostrum.Struct.Guild.Member.guild_permissions(member, guild)
  #=> [:administrator]
  ```
  """
  @spec guild_permissions(t, Guild.t()) :: [Permission.t()]
  def guild_permissions(member, guild)

  def guild_permissions(%__MODULE__{user: %{id: user_id}}, %Guild{owner_id: owner_id})
      when user_id === owner_id,
      do: Permission.all()

  def guild_permissions(%__MODULE__{} = member, %Guild{} = guild) do
    use Bitwise

    everyone_role_id = guild.id
    member_role_ids = member.roles ++ [everyone_role_id]

    member_permissions =
      member_role_ids
      |> Enum.map(&Map.get(guild.roles, &1))
      |> Enum.filter(&(!match?(nil, &1)))
      |> Enum.reduce(0, fn role, bitset_acc ->
        bitset_acc ||| role.permissions
      end)
      |> Permission.from_bitset()

    if Enum.member?(member_permissions, :administrator) do
      Permission.all()
    else
      member_permissions
    end
  end

  @doc """
  Returns a member's permissions in a guild channel, based on its `Nostrum.Struct.Overwrite`s.

  ## Examples

  ```Elixir
  guild = Nostrum.Cache.GuildCache.get!(279093381723062272)
  member = Map.get(guild.members, 177888205536886784)
  channel_id = 381889573426429952
  Nostrum.Struct.Guild.Member.guild_channel_permissions(member, guild, channel_id)
  #=> [:manage_messages]
  ```
  """
  @spec guild_channel_permissions(t, Guild.t(), Channel.id()) :: [Permission.t()]
  def guild_channel_permissions(%__MODULE__{} = member, guild, channel_id) do
    use Bitwise

    guild_perms = guild_permissions(member, guild)

    if Enum.member?(guild_perms, :administrator) do
      Permission.all()
    else
      channel = Map.get(guild.channels, channel_id)

      everyone_role_id = guild.id
      role_ids = [everyone_role_id | member.roles]
      overwrite_ids = role_ids ++ [member.user.id]

      {allow, deny} =
        channel.permission_overwrites
        |> Enum.filter(&(&1.id in overwrite_ids))
        |> Enum.map(fn overwrite -> {overwrite.allow, overwrite.deny} end)
        |> Enum.reduce({0, 0}, fn {allow, deny}, {allow_acc, deny_acc} ->
          {allow_acc ||| allow, deny_acc ||| deny}
        end)

      allow_perms = allow |> Permission.from_bitset()
      deny_perms = deny |> Permission.from_bitset()

      guild_perms
      |> Enum.reject(&(&1 in deny_perms))
      |> Enum.concat(allow_perms)
      |> Enum.dedup()
    end
  end

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
