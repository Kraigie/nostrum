defmodule Mixcord.Constructs.Guild do
  @moduledoc """
  Struct representing a Discord guild and various helper functions.

  ## List properties
  The guild struct contains multiple lists, including members and roles among others.
  These lists should not be relied upon to be accurate.
  They're only guaranteed to be accurate immediately following a GUILD_CREATE event.
  """

  #TODO:  Decide if we want recreate the struct when things like members are changed.
  #       In this way they will always be up to date.

  @doc """
  Defines the Guild struct.

  * :id - *String*. The guild's id.
  * :name - *String*. The guild's name.
  * :icon - *String*. Hash of guild's icon.
  * :splash - *String*. Hash of guild's splash.
  * :owner_id - *String*. Id of the guild owner.
  * :region - *String*. Id of the voice region.
  * :afk_channel_id - *String*. Id of the guild's afk channel.
  * :afk_timeout - *Integer*. How long someone must be afk before being moved.
  * :embed_enabled - *Boolean*. Is this guild embeddable.
  * :embed_channel_id - *String*. Id of the embedded channel.
  * :verification_level - *Integer*. Level of verification.
  * :default_message_notifications - *Integer*. Default message notification level.
  * :roles - *List*. List of `Mixcord.Constructs.Role` structs.
  * :emojis - *List*. List of [emojis](https://discordapp.com/developers/docs/resources/guild#emoji-object) as maps.
  * :features - *List*. List of guild features.
  * :mfa_level - *Integer*. Required MFA level of the guild.
  * :joined_at  - *Date*. Date the user joined the guild at.
  * :large  - *Boolean*. Whether the guild is considered "large".
  * :unavailable  - *Boolean*. Whether the guild is available.
  * :member_count - *Integer*. Total number of members in the guild.
  * :voice_states  - *List*. List of [voice states](https://discordapp.com/developers/docs/resources/voice#voice-state-object) as maps.
  * :members  - *List*. List of `Mixcord.Constructs.Member` structs.
  * :channels  - *List*. List of `Mixcord.Constructs.Channel` structs.
  * :presences  - *List*. List of simple presence maps.
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
    :embed_enabled,
    :embed_channel_id,
    :verification_level,
    :default_message_notifications,
    :roles,
    :emojis,
    :features,
    :mfa_level,
    :joined_at,
    :large,
    :unavailable,
    :member_count,
    :voice_states,
    :members,
    :channels,
    :presences,
  ]
end