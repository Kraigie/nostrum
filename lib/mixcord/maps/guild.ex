defmodule Mixcord.Map.Guild do
  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Mixcord.Map.{Member, TextChannel, Role}

  # TODO: Typedocs for all :>
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
  @type emojis :: list(Map.t)

  @typedoc "List of roles"
  @type roles :: list(Role.t)

  @typedoc "List of guild features"
  @type features :: list(Map.t)

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
  @type voice_states :: list(Map.t)

  @typedoc "List of members"
  @type members :: list(Member.t)

  @typedoc "List of channels"
  @type channels :: list(TextChannel.t)

  @typedoc "List of simple presence maps"
  @type presences :: list(Map.t)

  @type t :: Map.t

  @doc """
  Represents a Discord Guild.

  * `:name` - *String*. The guild's name.
  * `:icon` - *String*. Hash of guild's icon.
  * `:splash` - *String*. Hash of guild's splash.
  * `:owner_id` - *Integer*. Id of the guild owner.
  * `:region` - *String*. Id of the voice region.
  * `:afk_channel_id` - *Integer*. Id of the guild's afk channel.
  * `:afk_timeout` - *Integer*. How long someone must be afk before being moved.
  * `:embed_enabled` - *Boolean*. Is this guild embeddable.
  * `:embed_channel_id` - *Integer*. Id of the embedded channel.
  * `:verification_level` - *Integer*. Level of verification.
  * `:default_message_notifications` - *Integer*. Default message notification level.
  * `:roles` - *List*. List of `Mixcord.Map.Role` maps.
  * `:emojis` - *List*. List of [emojis](https://discordapp.com/developers/docs/resources/guild#emoji-object) as maps.
  * `:features` - *List*. List of guild features.
  * `:mfa_level` - *Integer*. Required MFA level of the guild.
  * `:joined_at` - *Date*. Date the user joined the guild at.
  * `:large ` - *Boolean*. Whether the guild is considered "large".
  * `:unavailable ` - *Boolean*. Whether the guild is available.
  * `:member_count` - *Integer*. Total number of members in the guild.
  * `:voice_states ` - *List*. List of [voice states](https://discordapp.com/developers/docs/resources/voice#voice-state-object) as maps.
  * `:members ` - *List*. List of `Mixcord.Map.Member` maps.
  * `:channels ` - *List*. List of `Mixcord.Map.Channel` maps.
  * `:presences ` - *List*. List of simple presence maps.
  """
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
    :joined_at, # TODO: How is this being calculated? I imagine it's a string, maybe we can parse it?
    :large,
    :unavailable,
    :member_count,
    :voice_states,
    :members,
    :presences,
    :channels
  ]
end
