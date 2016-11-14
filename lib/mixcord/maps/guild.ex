defmodule Mixcord.Map.Guild do
  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Mixcord.Map.{Channel}

  @type id :: integer
  @type name :: String.t
  @type icon :: String.t
  @type message :: %{
    :id => integer
  }

  @doc """
  Represents a Discord Guild.

  * `:id` - *Integer*. The guild's id.
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
  * `:joined_at ` - *Date*. Date the user joined the guild at.
  * `:large ` - *Boolean*. Whether the guild is considered "large".
  * `:unavailable ` - *Boolean*. Whether the guild is available.
  * `:member_count` - *Integer*. Total number of members in the guild.
  * `:voice_states ` - *List*. List of [voice states](https://discordapp.com/developers/docs/resources/voice#voice-state-object) as maps.
  * `:members ` - *List*. List of `Mixcord.Map.Member` maps.
  * `:channels ` - *List*. List of `Mixcord.Map.Channel` maps.
  * `:presences ` - *List*. List of simple presence maps.
  """
  @derive [Poison.Encoder]
  @type t :: %__MODULE__{
    id: integer,
    name: String.t,
    icon: String.t,
    splash: String.t,
    owner_id: integer,
    region: String.t,
    afk_channel_id: integer,
    afk_timeout: integer,
    embed_enabled: boolean,
    embed_channel_id: integer,
    verification_level: integer,
    default_message_notifications: integer,
    roles: list
  }
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