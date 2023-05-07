defmodule Nostrum.Struct.Guild.Member do
  @moduledoc ~S"""
  Struct representing a Discord guild member.

  A `Nostrum.Struct.Guild.Member` stores a `Nostrum.Struct.User`'s properties
  pertaining to a specific `Nostrum.Struct.Guild`.

  ## Mentioning Members in Messages

  A `Nostrum.Struct.Guild.Member` can be mentioned in message content using the `String.Chars`
  protocol or `mention/1`.

  ```elixir
  member = %Nostrum.Struct.Guild.Member{user_id: 120571255635181568}
  Nostrum.Api.create_message!(184046599834435585, "#{member}")
  %Nostrum.Struct.Message{content: "<@120571255635181568>"}

  member = %Nostrum.Struct.Guild.Member{user_id: 89918932789497856}
  Nostrum.Api.create_message!(280085880452939778, "#{Nostrum.Struct.Guild.Member.mention(member)}")
  %Nostrum.Struct.Message{content: "<@89918932789497856>"}
  ```
  """

  alias Nostrum.Permission
  alias Nostrum.Struct.{Channel, Guild, User}
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.{Snowflake, Util}
  import Bitwise

  defstruct [
    :user_id,
    :nick,
    :roles,
    :joined_at,
    :deaf,
    :mute,
    :communication_disabled_until,
    :premium_since
  ]

  defimpl String.Chars do
    def to_string(member), do: @for.mention(member)
  end

  @typedoc """
  The user ID.

  This field can be `nil` if the Member struct came as a partial Member object
  included in a message received from a guild channel. To retrieve the user
  object, use `Nostrum.Cache.UserCache`.
  """
  @type user_id :: User.id() | nil

  @typedoc "The nickname of the member"
  @type nick :: String.t() | nil

  @typedoc "A list of role ids"
  @type roles :: [Role.id()]

  @typedoc """
  Date the member joined the guild, as a unix timestamp.
  If you dont request offline guild members this field will be `nil` for any members that come online.
  """
  @type joined_at :: pos_integer() | nil

  @typedoc """
  Whether the member is deafened.
  If you dont request offline guild members this field will be `nil` for any members that come online.
  """
  @type deaf :: boolean | nil

  @typedoc """
  Whether the member is muted.
  If you dont request offline guild members this field will be `nil` for any members that come online.
  """
  @type mute :: boolean | nil

  @typedoc """
  Current timeout status of the member.

  If member is currently timed out this will be a `t:DateTime.t/0` of the
  unmute time, it will be `nil` or a date in the past if the member is not
  currently timed out.
  """
  @type communication_disabled_until :: DateTime.t() | nil

  @typedoc """
  Current guild booster status of the member.

  If member is currently boosting a guild this will be a `t:DateTime.t/0` since
  the start of the boosting, it will be `nil` if the member is not currently
  boosting the guild.
  """
  @type premium_since :: DateTime.t() | nil

  @type t :: %__MODULE__{
          user_id: user_id,
          nick: nick,
          roles: roles,
          joined_at: joined_at,
          deaf: deaf,
          mute: mute,
          communication_disabled_until: communication_disabled_until,
          premium_since: premium_since
        }

  @doc ~S"""
  Formats a `Nostrum.Struct.Guild.Member` into a mention.

  ## Examples

  ```elixir
  iex> member = %Nostrum.Struct.Guild.Member{user_id: 177888205536886784}
  ...> Nostrum.Struct.Guild.Member.mention(member)
  "<@177888205536886784>"
  ```
  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{user_id: user_id}) do
    "<@#{user_id}>"
  end

  @doc """
  Returns a member's guild permissions.

  ## Examples

  ```elixir
  guild = Nostrum.Cache.GuildCache.get!(279093381723062272)
  member = Map.get(guild.members, 177888205536886784)
  Nostrum.Struct.Guild.Member.guild_permissions(member, guild)
  #=> [:administrator]
  ```
  """
  @spec guild_permissions(t, Guild.t()) :: [Permission.t()]
  def guild_permissions(member, guild)

  def guild_permissions(%__MODULE__{user_id: owner_id}, %Guild{owner_id: owner_id})
      when owner_id != nil,
      do: Permission.all()

  def guild_permissions(%__MODULE__{} = member, %Guild{} = guild) do
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

  ```elixir
  guild = Nostrum.Cache.GuildCache.get!(279093381723062272)
  member = Map.get(guild.members, 177888205536886784)
  channel_id = 381889573426429952
  Nostrum.Struct.Guild.Member.guild_channel_permissions(member, guild, channel_id)
  #=> [:manage_messages]
  ```
  """
  @spec guild_channel_permissions(t, Guild.t(), Channel.id()) :: [Permission.t()]
  def guild_channel_permissions(%__MODULE__{} = member, guild, channel_id) do
    guild_perms = guild_permissions(member, guild)

    if Enum.member?(guild_perms, :administrator) do
      Permission.all()
    else
      channel = Map.get(guild.channels, channel_id)

      everyone_role_id = guild.id
      role_ids = [everyone_role_id | member.roles]
      overwrite_ids = role_ids ++ [member.user_id]

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

  @doc """
  Return the topmost role of the given member on the given guild.

  The topmost role is determined via `t:Nostrum.Struct.Guild.Role.position`.

  ## Parameters

  - `member`: The member whose top role to return.
  - `guild`: The guild which the member belongs to.

  ## Return value

  The topmost role of the member on the given guild, if the member has roles
  assigned. Otherwise, `nil` is returned.
  """
  @doc since: "0.5.0"
  @spec top_role(__MODULE__.t(), Guild.t()) :: Role.t() | nil
  def top_role(%__MODULE__{roles: member_roles}, %Guild{roles: guild_roles}) do
    guild_roles
    |> Stream.filter(fn {id, _role} -> id in member_roles end)
    |> Stream.map(fn {_id, role} -> role end)
    |> Enum.max_by(& &1.position, fn -> nil end)
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:roles, nil, &Util.cast(&1, {:list, Snowflake}))
      |> Map.update(:communication_disabled_until, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:premium_since, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:joined_at, nil, &Util.maybe_to_unixtime/1)
      |> Map.put(:user_id, Util.cast(map[:user][:id], Snowflake))

    struct(__MODULE__, new)
  end
end
