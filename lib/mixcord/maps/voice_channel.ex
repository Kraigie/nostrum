defmodule Mixcord.Map.VoiceChannel do
  @moduledoc """
  Struct representing a Discord voice channel.
  """
  @typedoc "The channe's id"
  @Type id :: integer

  @typedoc "The id of the channel's guild"
  @Type guild_id :: integer

  @typedoc "The name of the channel"
  @Type name :: String.t

  @typedoc "The type of channel: 'Text' or 'Voice'"
  @Type type :: String.t

  @typedoc "The ordered position of the channel"
  @Type position :: integer

  @typedoc "Whether the channel is private"
  @Type is_private :: boolean

  @typedoc "The list of overwrites"
  @Type permission_overwrites :: list(Map.t)

  @typedoc "The bitrate of the voice channel"
  @Type bitrate :: integer

  @typedoc "The user limit of the voice channel"
  @Type user_limit :: integer

  @Type t :: Map.t

  @doc """
  Represents a Discord Voice Channel.

  * `:id` - *Integer*. The channel's id.
  * `:guild_id` - *Integer*. Id of the channel's guild.
  * `:name` - *String*. Name of the channel
  * `:type` - *String*. `Text` or `Voice`
  * `:position` - *Integer*. Ordering position of the channel
  * `:is_private` - *Boolean*. If channel is a DM
  * `:permission_overwrites` - *List*. List of [overwrite](https://discordapp.com/developers/docs/resources/channel#overwrite-object) objects as maps
  * `:bitrate` - *Integer*. Bitrate of the voice channel.
  * `:user_limit` - *Integer*. User limit of the voice channel.
  """
  @derive [Poison.Encoder]
  defstruct [
    :id,
    :guild_id,
    :name,
    :type,
    :position,
    :is_private,
    :permission_overwrites,
    :bitrate,
    :user_limit
  ]
end
