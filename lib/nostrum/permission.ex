defmodule Nostrum.Permission do
  @moduledoc """
  Functions that work on permissions.

  Some functions return a list of permissions. You can use enumerable functions
  to work with permissions:

  ```Elixir
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Guild.Member

  guild = GuildCache.get!(279093381723062272)
  member = Map.get(guild.members, 177888205536886784)
  member_perms = Member.guild_permissions(member, guild)

  if :administrator in member_perms do
    IO.puts("This user has the administrator permission.")
  end
  ```
  """

  use Bitwise

  @typedoc """
  Represents a single permission as a bitvalue.
  """
  @type bit :: non_neg_integer

  @typedoc """
  Represents a set of permissions as a bitvalue.
  """
  @type bitset :: non_neg_integer

  @typedoc """
  This grouping is from an older version of the permission API and no longer applies
  """
  @typedoc deprecated: "See t.0"
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

  @typedoc """
  The permissions that apply to text channels.
  """
  @type text_permission ::
          :create_instant_invite
          | :manage_channels
          | :add_reactions
          | :view_channel
          | :send_messages
          | :send_tts_messages
          | :manage_messages
          | :embed_links
          | :attach_files
          | :read_message_history
          | :mention_everyone
          | :use_external_emojis
          | :manage_roles
          | :manage_webhooks
          | :use_application_commands
          | :manage_threads
          | :create_public_threads
          | :create_private_threads
          | :use_external_stickers
          | :send_messages_in_threads

  @typedoc """
  The permissions that apply to voice channels.
  """
  @type voice_permission ::
          :create_instant_invite
          | :manage_channels
          | :priority_speaker
          | :stream
          | :view_channel
          | :connect
          | :speak
          | :mute_members
          | :deafen_members
          | :move_members
          | :use_vad
          | :manage_roles

  @typedoc """
  The permissions that apply to stage channels.
  """
  @typedoc since: "0.5"
  @type stage_permission ::
          :create_instant_invite
          | :manage_channels
          | :view_channel
          | :connect
          | :mute_members
          | :deafen_members
          | :move_members
          | :manage_roles
          | :request_to_speak

  @typedoc """
  The permissions which apply to general server administration.
  """
  @typedoc since: "0.5"
  @type server_permission ::
          :kick_members
          | :ban_members
          | :administrator
          | :manage_guild
          | :view_audit_log
          | :view_guild_insights
          | :change_nickname
          | :manage_nicknames
          | :manage_emojis_and_stickers
  @typedoc """
  The full permission type.
  """
  @type t ::
          :create_instant_invite
          | :kick_members
          | :ban_members
          | :administrator
          | :manage_channels
          | :manage_guild
          | :add_reactions
          | :view_audit_log
          | :priority_speaker
          | :stream
          | :view_channel
          | :send_messages
          | :send_tts_messages
          | :manage_messages
          | :embed_links
          | :attach_files
          | :read_message_history
          | :mention_everyone
          | :use_external_emojis
          | :view_guild_insights
          | :connect
          | :speak
          | :mute_members
          | :deafen_members
          | :move_members
          | :use_vad
          | :change_nickname
          | :manage_nicknames
          | :manage_roles
          | :manage_webhooks
          | :manage_emojis_and_stickers
          | :use_application_commands
          | :request_to_speak
          | :manage_threads
          | :create_public_threads
          | :create_private_threads
          | :use_external_stickers
          | :send_messages_in_threads

  @permission_to_bit_map %{
    create_instant_invite: 0x0000000001,
    kick_members: 0x0000000002,
    ban_members: 0x0000000004,
    administrator: 0x0000000008,
    manage_channels: 0x0000000010,
    manage_guild: 0x0000000020,
    add_reactions: 0x0000000040,
    view_audit_log: 0x0000000080,
    priority_speaker: 0x0000000100,
    stream: 0x0000000200,
    view_channel: 0x0000000400,
    send_messages: 0x0000000800,
    send_tts_messages: 0x0000001000,
    manage_messages: 0x0000002000,
    embed_links: 0x0000004000,
    attach_files: 0x0000008000,
    read_message_history: 0x0000010000,
    mention_everyone: 0x0000020000,
    use_external_emojis: 0x0000040000,
    view_guild_insights: 0x0000080000,
    connect: 0x0000100000,
    speak: 0x0000200000,
    mute_members: 0x0000400000,
    deafen_members: 0x0000800000,
    move_members: 0x0001000000,
    use_vad: 0x0002000000,
    change_nickname: 0x0004000000,
    manage_nicknames: 0x0008000000,
    manage_roles: 0x0010000000,
    manage_webhooks: 0x0020000000,
    manage_emojis_and_stickers: 0x0040000000,
    use_application_commands: 0x0080000000,
    request_to_speak: 0x0100000000,
    manage_threads: 0x0400000000,
    create_public_threads: 0x0800000000,
    create_private_threads: 0x1000000000,
    use_external_stickers: 0x2000000000,
    send_messages_in_threads: 0x4000000000
  }

  @bit_to_permission_map Map.new(@permission_to_bit_map, fn {k, v} -> {v, k} end)
  @permission_list Map.keys(@permission_to_bit_map)

  @doc """
  Returns `true` if `term` is a permission; otherwise returns `false`.

  ## Examples

  ```Elixir
  iex> Nostrum.Permission.is_permission(:administrator)
  true

  iex> Nostrum.Permission.is_permission(:not_a_permission)
  false
  ```
  """
  defguard is_permission(term) when is_atom(term) and term in @permission_list

  @doc """
  Returns a list of all permissions.
  """
  @spec all() :: [t]
  def all, do: @permission_list

  @doc """
  Converts the given bit to a permission.

  This function returns `:error` if `bit` does not map to a permission.

  ## Examples

  ```Elixir
  iex> Nostrum.Permission.from_bit(0x04000000)
  {:ok, :change_nickname}

  iex> Nostrum.Permission.from_bit(0)
  :error
  ```
  """
  @spec from_bit(bit) :: {:ok, t} | :error
  def from_bit(bit) do
    Map.fetch(@bit_to_permission_map, bit)
  end

  @doc """
  Same as `from_bit/1`, but raises `ArgumentError` in case of failure.

  ## Examples

  ```Elixir
  iex> Nostrum.Permission.from_bit!(0x04000000)
  :change_nickname

  iex> Nostrum.Permission.from_bit!(0)
  ** (ArgumentError) expected a valid bit, got: `0`
  ```
  """
  @spec from_bit!(bit) :: t
  def from_bit!(bit) do
    case from_bit(bit) do
      {:ok, perm} -> perm
      :error -> raise(ArgumentError, "expected a valid bit, got: `#{inspect(bit)}`")
    end
  end

  @doc """
  Converts the given bitset to a list of permissions.

  If invalid bits are given they will be omitted from the results.

  ## Examples

  ```Elixir
  iex> Nostrum.Permission.from_bitset(0x08000002)
  [:manage_nicknames, :kick_members]

  iex> Nostrum.Permission.from_bitset(0x4000000000000)
  []
  ```
  """
  @spec from_bitset(bitset) :: [t]
  def from_bitset(bitset) do
    0..53
    |> Enum.map(fn index -> 0x1 <<< index end)
    |> Enum.filter(fn mask -> (bitset &&& mask) === mask end)
    |> Enum.reduce([], fn bit, acc ->
      case from_bit(bit) do
        {:ok, perm} -> [perm | acc]
        :error -> acc
      end
    end)
  end

  @doc """
  Converts the given permission to a bit.

  ## Examples

  ```Elixir
  iex> Nostrum.Permission.to_bit(:administrator)
  8
  ```
  """
  @spec to_bit(t) :: bit
  def to_bit(permission) when is_permission(permission), do: @permission_to_bit_map[permission]

  @doc """
  Converts the given enumerable of permissions to a bitset.

  ## Examples

  ```Elixir
  iex> Nostrum.Permission.to_bitset([:administrator, :create_instant_invite])
  9
  ```
  """
  @spec to_bitset(Enum.t()) :: bitset
  def to_bitset(permissions) do
    permissions
    |> Enum.map(&to_bit(&1))
    |> Enum.reduce(fn bit, acc -> acc ||| bit end)
  end
end
