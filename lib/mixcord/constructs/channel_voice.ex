defmodule Mixcord.Constructs.VoiceChannel do
  @moduledoc """
  Struct representing a Discord voice channel and various helper functions.
  """

  @doc """
  Defines the Voice Channel struct.

  * :id - *String*. The channel's id.
  * :guild_id - *String*. Id of the channel's guild.
  * :name - *String*. Name of the channel
  * :type - *String*. `Text` or `Voice`
  * :position - *Integer*. Ordering position of the channel
  * :is_private - *Boolean*. If channel is a DM
  * :permission_overwrites - *List*. List of [overwrite](https://discordapp.com/developers/docs/resources/channel#overwrite-object) objects as maps
  * :bitrate - *Integer*. Bitrate of the voice channel.
  * :user_limit - *Integer*. User limit of the voice channel.
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