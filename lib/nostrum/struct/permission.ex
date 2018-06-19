defmodule Nostrum.Struct.Permission do
  @moduledoc """
  Functions that work on permissions.

  Some functions return permission sets. Permission sets are `MapSet`s with
  atoms representing permissions. You can use the `MapSet` module to check for
  permissions, as follows:

  ```Elixir
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Guild.Member

  guild = GuildCache.get!(279093381723062272)
  member = Enum.find(guild.members, & &1.id === 177888205536886784)
  member_perms = Member.guild_permissions(member, guild)

  if MapSet.member?(member_perms, :administrator) do
    IO.puts("This user has the administrator permission.")
  end
  ```
  """

  use Bitwise

  @type bitset :: integer
  @type general_permission ::
          :create_instant_invite
          | :kick_members
          | :ban_members
          | :administrator
          | :manage_channels
          | :manage_guild
          | :view_audit_log
          | :view_channel
          | :change_nickname
          | :manage_nicknames
          | :manage_roles
          | :manage_webhooks
          | :manage_emojis

  @type text_permission ::
          :add_reactions
          | :send_messages
          | :send_tts_messages
          | :manage_messages
          | :embed_links
          | :attach_files
          | :read_message_history
          | :mention_everyone
          | :use_external_emojis

  @type voice_permission ::
          :connect
          | :speak
          | :mute_members
          | :deafen_members
          | :move_members
          | :use_vad

  @type t ::
          general_permission
          | text_permission
          | voice_permission

  @type permission_set :: MapSet.t(t)

  @permission_to_bitvalue_map %{
    create_instant_invite: 0x00000001,
    kick_members: 0x00000002,
    ban_members: 0x00000004,
    administrator: 0x00000008,
    manage_channels: 0x00000010,
    manage_guild: 0x00000020,
    add_reactions: 0x00000040,
    view_audit_log: 0x00000080,
    view_channel: 0x00000400,
    send_messages: 0x00000800,
    send_tts_messages: 0x00001000,
    manage_messages: 0x00002000,
    embed_links: 0x00004000,
    attach_files: 0x00008000,
    read_message_history: 0x00010000,
    mention_everyone: 0x00020000,
    use_external_emojis: 0x00040000,
    connect: 0x00100000,
    speak: 0x00200000,
    mute_members: 0x00400000,
    deafen_members: 0x00800000,
    move_members: 0x01000000,
    use_vad: 0x02000000,
    change_nickname: 0x04000000,
    manage_nicknames: 0x08000000,
    manage_roles: 0x10000000,
    manage_webhooks: 0x20000000,
    manage_emojis: 0x40000000
  }

  @permission_list Map.keys(@permission_to_bitvalue_map)

  @doc """
  Returns `true` if `term` is a permission; otherwise returns `false`.

  ## Examples

  ```Elixir
  iex> Nostrum.Struct.Permission.is_permission(:administrator)
  true

  iex> Nostrum.Struct.Permission.is_permission(:not_a_permission)
  false
  ```
  """
  defguard is_permission(term) when is_atom(term) and term in @permission_list

  @doc """
  Returns a permission set containing all permissions.
  """
  @spec all() :: permission_set
  def all do
    @permission_to_bitvalue_map
    |> Enum.map(fn {perm, _} -> perm end)
    |> MapSet.new()
  end

  @doc """
  Converts the given bitset to a permission set.

  ## Examples

  ```Elixir
  iex> Nostrum.Struct.Permission.from_bitset(0x04000000)
  #MapSet<[:change_nickname]>

  iex> Nostrum.Struct.Permission.from_bitset(0x08000002)
  #MapSet<[:kick_members, :manage_nicknames]>

  iex> Nostrum.Struct.Permission.from_bitset(0)
  #MapSet<[]>
  ```
  """
  @spec from_bitset(bitset) :: permission_set
  def from_bitset(bitset) do
    Enum.reduce(@permission_to_bitvalue_map, [], fn {perm, bitvalue}, acc ->
      if band(bitset, bitvalue) == bitvalue do
        [perm | acc]
      else
        acc
      end
    end)
    |> MapSet.new()
  end

  @doc """
  Converts the given permission set to a bitset.

  ## Examples

  ```Elixir
  iex> Nostrum.Struct.Permission.all
  ...> |> Nostrum.Struct.Permission.to_bitset()
  2146958591
  ```
  """
  @spec to_bitset(permission_set) :: bitset
  def to_bitset(permission_set) do
    Enum.reduce(permission_set, 0, fn perm, acc ->
      acc ||| @permission_to_bitvalue_map[perm]
    end)
  end
end
