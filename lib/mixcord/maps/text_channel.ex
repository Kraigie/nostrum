defmodule Mixcord.Map.TextChannel do
  @moduledoc """
  Struct representing a Discord text channel.
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

  @typedoc "Current channel topic"
  @Type topic :: String.t

  @typedoc "Id of the last message sent"
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
