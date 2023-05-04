defmodule Nostrum.Permission do
  @moduledoc """
  Functions that work on permissions.

  Some functions return a list of permissions. You can use enumerable functions
  to work with permissions:

  ```elixir
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

  import Bitwise

  @typedoc """
  Represents a single permission as a bitvalue.
  """
  @type bit :: non_neg_integer

  @typedoc """
  Represents a set of permissions as a bitvalue.
  """
  @type bitset :: non_neg_integer

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
          | :manage_emojis_and_stickers
          | :view_guild_insights
          | :use_application_commands
          | :moderate_members

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
          | :create_public_threads
          | :create_private_threads
          | :send_messages_in_threads
          | :manage_threads
          | :use_external_stickers

  @type voice_permission ::
          :connect
          | :speak
          | :mute_members
          | :deafen_members
          | :move_members
          | :use_vad
          | :priority_speaker
          | :stream
          | :request_to_speak
          | :manage_events
          | :use_embedded_activities

  @type t ::
          general_permission
          | text_permission
          | voice_permission

  @permission_to_bit_map %{
    create_instant_invite: 1 <<< 0,
    kick_members: 1 <<< 1,
    ban_members: 1 <<< 2,
    administrator: 1 <<< 3,
    manage_channels: 1 <<< 4,
    manage_guild: 1 <<< 5,
    add_reactions: 1 <<< 6,
    view_audit_log: 1 <<< 7,
    priority_speaker: 1 <<< 8,
    stream: 1 <<< 9,
    view_channel: 1 <<< 10,
    send_messages: 1 <<< 11,
    send_tts_messages: 1 <<< 12,
    manage_messages: 1 <<< 13,
    embed_links: 1 <<< 14,
    attach_files: 1 <<< 15,
    read_message_history: 1 <<< 16,
    mention_everyone: 1 <<< 17,
    use_external_emojis: 1 <<< 18,
    view_guild_insights: 1 <<< 19,
    connect: 1 <<< 20,
    speak: 1 <<< 21,
    mute_members: 1 <<< 22,
    deafen_members: 1 <<< 23,
    move_members: 1 <<< 24,
    use_vad: 1 <<< 25,
    change_nickname: 1 <<< 26,
    manage_nicknames: 1 <<< 27,
    manage_roles: 1 <<< 28,
    manage_webhooks: 1 <<< 29,
    manage_emojis_and_stickers: 1 <<< 30,
    use_application_commands: 1 <<< 31,
    request_to_speak: 1 <<< 32,
    manage_events: 1 <<< 33,
    manage_threads: 1 <<< 34,
    create_public_threads: 1 <<< 35,
    create_private_threads: 1 <<< 36,
    use_external_stickers: 1 <<< 37,
    send_messages_in_threads: 1 <<< 38,
    use_embedded_activities: 1 <<< 39,
    moderate_members: 1 <<< 40
  }

  @bit_to_permission_map Map.new(@permission_to_bit_map, fn {k, v} -> {v, k} end)
  @permission_list Map.keys(@permission_to_bit_map)

  @doc """
  Returns `true` if `term` is a permission; otherwise returns `false`.

  ## Examples

  ```elixir
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

  ```elixir
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

  ```elixir
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

  ```elixir
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

  ```elixir
  iex> Nostrum.Permission.to_bit(:administrator)
  8
  ```
  """
  @spec to_bit(t) :: bit
  def to_bit(permission) when is_permission(permission), do: @permission_to_bit_map[permission]

  @doc """
  Converts the given enumerable of permissions to a bitset.

  ## Examples

  ```elixir
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
