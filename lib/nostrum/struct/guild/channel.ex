defmodule Nostrum.Struct.Guild.Channel do
  @moduledoc """
  Struct representing a Discord channel.
 
  A `Nostrum.Struct.Guild.Channel` represents all 7 types of channels. Each 
  channel has a field `:type` with any of the following values:
 
    * `0` - GUILD_TEXT
    * `1` - DM
    * `2` - GUILD_VOICE
    * `3` - GROUP_DM
    * `4` - GUILD_CATEGORY
 
  More information can be found on the 
  [Discord API Channel Documentation](https://discordapp.com/developers/docs/resources/channel#channel-object).
 
  ## Guild Text Channels
 
  Guild text channels are channels that discord users often message through in 
  guilds. A guild text channel can be identified via pattern matching its `:type` field:
 
      {:ok, channel} = Nostrum.Api.get_channel(41771983423143933)
      %Nostrum.Struct.Guild.Channel{type: 0} = channel
 
  They have the following guaranteed fields:
 
    * `:id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:type` (integer)
    * `:guild_id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:position` (integer)
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0`)
    * `:nsfw` (boolean)
    * `:topic` (string)
    * `:last_message_id` (`t:Nostrum.Struct.Snowflake.t/0` or nil)
    * `:parent_id` (`t:Nostrum.Struct.Snowflake.t/0` or nil)
    * `:last_pin_timestamp` (string or nil)
 
  ## DM Channels
 
  DM channels are private messaging channels for one-on-one messaging. A 
  DM channel can be identified via pattern matching its `:type` field:
 
      {:ok, channel} = Nostrum.Api.create_dm(177888205536886784)
      %Nostrum.Struct.Guild.Channel{type: 1} = channel
 
  They have the following guaranteed fields:
 
    * `:id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:type` (integer)
    * `:last_message_id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:recipients` (list of `t:Nostrum.Struct.User.t/0`)
 
  ## Guild Voice Channels
 
  Guild voice channels are channels that users speak through on guilds. 
  A guild voice channel can be identified via pattern matching its `:type` field:
 
      {:ok, channel} = Nostrum.Api.get_channel(41771983423143933)
      %Nostrum.Struct.Guild.Channel{type: 2} = channel
 
  They have the following guaranteed fields:
 
    * `:id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:type` (integer)
    * `:guild_id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:position` (integer)
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0`)
    * `:name` (string)
    * `:nsfw` (boolean)
    * `:bitrate` (integer)
    * `:user_limit` (integer)
    * `:parent_id` (`t:Nostrum.Struct.Snowflake.t/0` or nil)
 
  ## Group DM Channels
 
  Group DM channels are channels that allow multiple users to privately message  
  together. A group DM channel can be identified via pattern matching its `:type` field:
 
      {:ok, channel} = Nostrum.Api.create_group_dm(["6qrZcUqja7812RVdnEKjpzOL4CvHBFG"], %{})
      %Nostrum.Struct.Guild.Channel{type: 3} = channel
 
  They have the following guaranteed fields:
 
    * `:id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:type` (integer)
    * `:name` (string)
    * `:last_message_id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:recipients` (list of `t:Nostrum.Struct.User.t/0`)
    * `:icon` (string or nil)
    * `:owner_id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:application_id` (`t:Nostrum.Struct.Snowflake.t/0` or nil)
  
  ## Channel Categories
 
  Channel Categories are fake channels used to represent categories in guilds. 
  A channel category can be identified via pattern matching its `:type` field:
 
      {:ok, channel} = Nostrum.Api.get_channel(41771983423143933)
      %Nostrum.Struct.Guild.Channel{type: 4} = channel
 
  They have the following guaranteed fields:
 
    * `:id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:type` (integer)
    * `:guild_id` (`t:Nostrum.Struct.Snowflake.t/0`)
    * `:position` (integer)
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0`)
    * `:name` (string)
    * `:nsfw` (boolean)
    * `:parent_id` (`t:Nostrum.Struct.Snowflake.t/0` or nil)
  """

  alias Nostrum.Struct.Overwrite
  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util

  defstruct [
    :id,
    :type,
    :guild_id,
    :position,
    :name,
    :topic,
    :nsfw,
    :last_message_id,
    :bitrate,
    :user_limit,
    :icon,
    :owner_id,
    :application_id,
    :parent_id,
    :last_pin_timestamp,
    permission_overwrites: [],
    recipients: []
  ]

  @typedoc "The channel's id"
  @type id :: Snowflake.t

  @typedoc "The type of the channel"
  @type type :: integer

  @typedoc "The id of the channel's guild"
  @type guild_id :: Snowflake.t | nil

  @typedoc "The ordered position of the channel"
  @type position :: integer | nil

  @typedoc "The list of overwrites"
  @type permission_overwrites :: [Overwrite.t]

  @typedoc "The name of the channel"
  @type name :: String.t | nil

  @typedoc "Current channel topic"
  @type topic :: String.t | nil

  @typedoc "If the channel is nsfw"
  @type nsfw :: boolean | nil

  @typedoc "Id of the last message sent"
  @type last_message_id :: Snowflake.t | nil

  @typedoc "The bitrate of the voice channel"
  @type bitrate :: integer | nil

  @typedoc "The user limit of the voice channel"
  @type user_limit :: integer | nil

  @typedoc "The recipients of the DM"
  @type recipients :: [User.t]

  @typedoc "The icon hash of the channel"
  @type icon :: String.t | nil

  @typedoc "The id of the DM creator"
  @type owner_id :: Snowflake.t | nil

  @typedoc "The application id of the group DM creator if it is bot-created"
  @type application_id :: Snowflake.t | nil

  @typedoc "The id of the parent category for a channel"
  @type parent_id :: Snowflake.t | nil

  @typedoc "When the last pinned message was pinned"
  @type last_pin_timestamp :: String.t | nil

  @type t :: %__MODULE__{
    id: id,
    type: type,
    guild_id: guild_id,
    position: position,
    permission_overwrites: permission_overwrites,
    name: name,
    topic: topic,
    nsfw: nsfw,
    last_message_id: last_message_id,
    bitrate: bitrate,
    user_limit: user_limit,
    recipients: recipients,
    icon: icon,
    owner_id: owner_id,
    application_id: application_id,
    parent_id: parent_id,
    last_pin_timestamp: last_pin_timestamp
  }

  def p_encode do
    %__MODULE__{
      permission_overwrites: [Overwrite.p_encode]
    }
  end

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
    |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
    |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
    |> Map.update(:permission_overwrites, [], &Util.cast(&1, {:list, {:struct, Overwrite}}))
    |> Map.update(:last_message_id, nil, &Util.cast(&1, Snowflake))
    |> Map.update(:recipients, [], &Util.cast(&1, {:list, {:struct, User}}))
    |> Map.update(:owner_id, nil, &Util.cast(&1, Snowflake))
    |> Map.update(:application_id, nil, &Util.cast(&1, Snowflake))
    |> Map.update(:parent_id, nil, &Util.cast(&1, Snowflake))
  end
end
