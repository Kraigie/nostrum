defmodule Nostrum.Struct.Guild do
  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild.{Channel, Member, Role, UnavailableGuild}
  alias Nostrum.Struct.VoiceState
  alias Nostrum.Util

  @typedoc "The guild's id"
  @type id :: integer

  @typedoc "The name of the guild."
  @type name :: String.t

  @typedoc "The hash of the guild's icon"
  @type icon :: String.t

  @typedoc "The hash of the guild's splash"
  @type splash :: String.t

  @typedoc "The id of the guild owner"
  @type owner_id :: integer

  @typedoc "The id of the voice region"
  @type region :: String.t

  @typedoc "The id of the guild's afk channel"
  @type afk_channel_id :: integer

  @typedoc "The time someone must be afk before being moved"
  @type afk_timeout :: integer

  @typedoc "Whether the guild is emeddable"
  @type embed_enabled :: boolean

  @typedoc "The id of the embedded channel"
  @type embed_channel_id :: integer

  @typedoc "The level of verification"
  @type verification_level :: integer

  @typedoc "Level of verification"
  @type default_message_notifications :: integer

  @typedoc "List of emojis as maps"
  @type emojis :: list(map)

  @typedoc "List of roles"
  @type roles :: list(Role.t)

  @typedoc "List of guild features"
  @type features :: list(map)

  @typedoc "Required MFA level of the guild"
  @type mfa_level :: integer

  @typedoc "Date the user joined the guild at"
  @type joined_at :: String.t

  @typedoc "Whether the guild is considered 'large'"
  @type large :: boolean

  @typedoc "Whether the guild is avaliable"
  @type unavailable :: boolean

  @typedoc "Total number of members in the guild"
  @type member_count :: integer

  @typedoc "List of voice states as maps"
  @type voice_states :: list(VoiceState.t)

  @typedoc "List of members"
  @type members :: list(Member.t)

  @typedoc "List of channels"
  @type channels :: list(Channel.t)

  @typedoc "List of simple presence maps"
  @type presences :: list(map)

  @type t :: %__MODULE__{
    id: id,
    name: name,
    icon: icon,
    splash: splash,
    owner_id: owner_id,
    region: region,
    afk_channel_id: afk_channel_id,
    afk_timeout: afk_timeout,
    embed_enabled: embed_enabled,
    embed_channel_id: embed_channel_id,
    verification_level: verification_level,
    default_message_notifications: default_message_notifications,
    roles: roles,
    emojis: emojis,
    features: features,
    mfa_level: mfa_level,
    joined_at: joined_at,
    large: large,
    unavailable: unavailable,
    member_count: member_count,
    voice_states: voice_states,
    members: members,
    presences: presences,
    channels: channels
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :name,
    :icon,
    :splash,
    :owner_id,
    :region,
    :afk_channel_id,
    :afk_timeout,
    :embed_enabled,
    :embed_channel_id,
    :verification_level,
    :default_message_notifications,
    :roles,
    :emojis,
    :features,
    :mfa_level,
    :joined_at, # REVIEW: How is this being calculated? I imagine it's a string, maybe we can parse it?
    :large,
    :unavailable,
    :member_count,
    :voice_states,
    :members,
    :presences,
    :channels
  ]

  @doc false
  def p_encode do
    %__MODULE__{
      emojis: [Emoji.p_encode],
      roles: [Role.p_encode],
      members: [Member.p_encode],
      channels: [Channel.p_encode],
      voice_states: [VoiceState.p_encode]
    }
  end

  def to_struct(%{unavailable: true} = map), do: UnavailableGuild.to_struct(map)
  def to_struct(map) do
    new = map
    |> Map.update(:emojis, [], &Util.list_to_struct_list(&1, Emoji))
    |> Map.update(:roles, [], &Util.list_to_struct_list(&1, Role))
    |> Map.update(:members, [], &Util.list_to_struct_list(&1, Member))
    |> Map.update(:channels, [], &Util.list_to_struct_list(&1, Channel))
    |> Map.update(:voice_states, %{}, &Util.list_to_struct_list(&1, VoiceState))
    struct(__MODULE__, new)
  end
end
