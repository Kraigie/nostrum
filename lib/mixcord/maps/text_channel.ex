defmodule Mixcord.Map.TextChannel do
  @moduledoc """
  Struct representing a Discord text channel.
  """

  @Type id :: integer
  @Type guild_id :: integer
  @Type name :: String.t
  @Type type :: String.t
  @Type position :: integer
  @Type is_private :: boolean
  @Type permission_overwrites :: list(Map.t)
  @Type topic :: String.t
  @Type last_message_id :: integer

  @Type t :: Map.t

  @doc """
  Represents a Discord Text Channel.

  * `:id` - *Integer*. The channel's id.
  * `:guild_id` - *Integer*. Id of the channel's guild.
  * `:name` - *String*. Name of the channel
  * `:type` - *String*. `Text` or `Voice`
  * `:position` - *Integer*. Ordering position of the channel
  * `:is_private` - *Boolean*. If channel is a DM
  * `:permission_overwrites` - *List*. List of [overwrite](https://discordapp.com/developers/docs/resources/channel#overwrite-object) objects as maps
  * `:topic` - *String*. Current channel topic.
  * `:last_message_id` - *Integer*. Id of the last message sent in the channel
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
    :topic,
    :last_message_id
  ]
end
