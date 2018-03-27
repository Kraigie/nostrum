defmodule Nostrum.Struct.Guild do
  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild.{Channel, Member, Role}
  require Nostrum.Util

  @typedoc "The guild's id"
  @type id :: integer

  @typedoc "The name of the guild."
  @type name :: String.t()

  @typedoc "The hash of the guild's icon"
  @type icon :: String.t()

  @typedoc "The hash of the guild's splash"
  @type splash :: String.t()

  @typedoc "The id of the guild owner"
  @type owner_id :: integer

  @typedoc "The id of the voice region"
  @type region :: String.t()

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

  @typedoc "List of roles, indexed by `t:Nostrum.Struct.Guild.Role.id/0`"
  @type roles :: map

  @typedoc "List of guild features"
  @type features :: list(map)

  @typedoc "Required MFA level of the guild"
  @type mfa_level :: integer

  @typedoc "Date the user joined the guild at"
  @type joined_at :: String.t()

  @typedoc "Whether the guild is considered 'large'"
  @type large :: boolean

  @typedoc "Whether the guild is avaliable"
  @type unavailable :: boolean

  @typedoc "Total number of members in the guild"
  @type member_count :: integer

  @typedoc "List of voice states as maps"
  @type voice_states :: list(map)

  @typedoc "List of members, indexed by `t:Nostrum.Struct.User.id/0`"
  @type members :: map

  @typedoc "List of channels, indexed by `t:Nostrum.Struct.Guild.Channel.id/0`"
  @type channels :: map

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

  Nostrum.Util.nostrum_struct(%{
    id: nil,
    name: nil,
    icon: nil,
    splash: nil,
    owner_id: nil,
    region: nil,
    afk_channel_id: nil,
    afk_timeout: nil,
    embed_enabled: nil,
    embed_channel_id: nil,
    verification_level: nil,
    default_message_notifications: nil,
    roles: [Role],
    emojis: [Emoji],
    features: nil,
    mfa_level: nil,
    # REVIEW: How is this being calculated? I imagine it's a string, maybe we can parse it?
    joined_at: nil,
    large: nil,
    unavailable: nil,
    member_count: nil,
    voice_states: nil,
    members: [Member],
    presences: nil,
    channels: [Channel]
  })
end
