defmodule Mixcord.Map.TextChannel do
  @moduledoc """
  Struct representing a Discord text channel.
  """

  @typedoc "The channe's id"
  @type id :: integer

  @typedoc "The id of the channel's guild"
  @type guild_id :: integer

  @typedoc "The name of the channel"
  @type name :: String.t

  @typedoc "The type of channel: 'Text' or 'Voice'"
  @type type :: String.t

  @typedoc "The ordered position of the channel"
  @type position :: integer

  @typedoc "Whether the channel is private"
  @type is_private :: boolean

  @typedoc "The list of overwrites"
  @type permission_overwrites :: list(Map.t)

  @typedoc "Current channel topic"
  @type topic :: String.t

  @typedoc "Id of the last message sent"
  @type last_message_id :: integer

  @type t :: Map.t

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
