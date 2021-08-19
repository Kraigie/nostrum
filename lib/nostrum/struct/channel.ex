defmodule Nostrum.Struct.Channel do
  @moduledoc """
  Struct representing a channel.

  There are currently eleven channel types within discord, ranging from direct messages between two users to server announcement channels pushing a message to thousands. This module also contains several helper functions for handling channels within your applcation.

  ## Channel Struct

  The channel struct is used by Nostrum to represent a _Discord Channel Object_. More information can be found on the [Discord API Channel Documentation](https://discord.com/developers/docs/resources/channel#channels-resource).

  The struct can have one of several forms depending on the type of channel. You can read more about the individual channel types [below](#module-channel-types).

  A typical channel would appear as:

  ```elixir
  channel = %Nostrum.Struct.Channel{
      guild_id: 766435015768539156,
      id: 827333533688397865,
      name: "announcements",
      nsfw: false,
      permission_overwrites: [],
      position: 1,
      type: 5,
      }
  ```

  The channel struct implements `String.Chars` protocol through the `mention/1` function. This example uses our channel from the previous code block.

  ```elixir
  channel |> to_string()
  "<#766435015768539156>"
  ```


  ## Channel Cache

  The [`ChannelCache`](`Nostrum.Cache.ChannelCache`) module provides functionality for you to retreive information about any channel that your application can see. It provides two functions: [`get()/1`](`Nostrum.Cache.ChannelCache.get()/1`) and [`get!()/1`](`Nostrum.Cache.ChannelCache.get!()/1`).

  ```elixir
  Nostrum.Cache.ChannelCache.get!(827333533688397865)
      %Nostrum.Struct.Channel{
            application_id: nil,
            bitrate: nil,
            guild_id: 766435015768539156,
            icon: nil,
            id: 827333533688397865,
            last_message_id: nil,
            last_pin_timestamp: nil,
            name: "announcements",
            nsfw: false,
            owner_id: nil,
            parent_id: nil,
            permission_overwrites: [
              %Nostrum.Struct.Overwrite{
                allow: 0,
                deny: 2048,
                id: 766435015768539156,
                type: :role
              }
            ],
            position: 1,
            recipients: nil,
            topic: nil,
            type: 5,
            user_limit: nil
          }
  ```

  More details of the cache can be found at `Nostrum.Cache.ChannelCache`.

  ## Helper Functions

  This module contains two functions for assisting with channel structs. `mention/1` to convert the channel into a mention as a string, and `link/1` to convert the channel into a hyperlink as a string. Further details and examples are detailed in the [Functions section of this module.](#functions)

  ## Api Functions

  The Nostrum Api contains numerous functions related to channels. Notable functions relating to channels are shown below.

  - `Nostrum.Api.create_guild_channel/2`
  - `Nostrum.Api.get_channel/1`
  - `Nostrum.Api.modify_channel/3`
  - `Nostrum.Api.delete_channel/2`
  - `Nostrum.Api.add_pinned_channel_message/2`
  - `Nostrum.Api.create_channel_invite/3`
  - `Nostrum.Api.get_guild_channels/1`
  - `Nostrum.Api.modify_guild_channel_positions/2`

  > Note: This is not an exhaustive list, for full details please see the `Nostrum.Api` module documentation.

  ## Channel Types
  Channels take the shape of various types depending on their use and not all fields are always used. The currently implemented channel types are detailed below. The type of channel is determined by the `:type` field.

  This diagram represents the regular channel types `0`, `2`, `5` and `13`.

    ![Discord Channels](http://puu.sh/I4uW4/21e27a3385.png)

    The currently implemented channel types are:

  |      | Channel Type         |                                                                                        |
  | ---- | -------------------- | -------------------------------------------------------------------------------------- |
  | `0`  | Guild Text           | `GUILD_TEXT`            _A text channel within aserver_                               |
  | `1`  | Direct Message       | `DM`                    _A direct message between users_                               |
  | `2`  | Guild Voice          | `GUILD_VOICE`           _A voice channel within a server_                              |
  | `3`  | Group Direct Message | `GROUP_DM`              _A direct message between multiple users_                      |
  | `4`  | Guild Category       | `GUILD_CATEGORY`        _A category that contains up to 50 channels_                   |
  | `5`  | Guild Announcements  | `GUILD_NEWS`            _A channel that users can follow and crosspost from_           |
  | `6`  | Guild Store          | `GUILD_STORE`           _A channel in which developers can sell their game on Discord_ |
  | `10` | Guild News Thread    | `GUILD_NEWS_THREAD`     _A temporary sub-channel within a `5` channel_                 |
  | `11` | Guild Public Thread  | `GUILD_PUBLIC_THREAD`   _A temporary sub-channel within an `0` channel_                |
  | `12` | Guild Private Thread | `GUILD_PRIVATE_THREAD`  _A temporary private sub-channel within an `0`                 |
  | `13` | Guild Stage Voice    | `GUILD_STAGE_VOICE`     _A voice channel for hosting events with an audience_          |



  More information about _Discord Channel Types_ can be found on the [Discord API Channel Type Documentation](https://discord.com/developers/docs/resources/channel#channel-object-channel-types).
  """

  defimpl String.Chars do
    @spec to_string(Nostrum.Struct.Channel.t()) :: String.t()
    def to_string(channel), do: @for.mention(channel)
  end

  alias Nostrum.Struct.{Channel, Guild, Message, Overwrite, User}
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :type,
    :guild_id,
    :position,
    :permission_overwrites,
    :name,
    :topic,
    :nsfw,
    :last_message_id,
    :bitrate,
    :user_limit,
    :rate_limit_per_user,
    :recipients,
    :icon,
    :owner_id,
    :application_id,
    :parent_id,
    :last_pin_timestamp,
    :rtc_region,
    :video_quality_mode,
    :message_count,
    :member_count,
    :thread_metadata,
    :member,
    :default_auto_archive_duration,
    :permissions
  ]

  ######
  ## loses a lot of information. to_string() would be expected to return the full channel details
  ## mention and link protocols could be defined?
  #####

  defimpl String.Chars do
    @spec to_string(Nostrum.Struct.Channel.t()) :: String.t()
    def to_string(channel), do: @for.mention(channel)
  end

  @typedoc """
  The id of the channel object.
  """
  @type id :: Snowflake.t()

  @typedoc """
  The type of channel.

  |      | Channel Type         |                                                                                        |
  | ---- | -------------------- | -------------------------------------------------------------------------------------- |
  | `0`  | Guild Text           | `GUILD_TEXT`            _A text channel within a server_                               |
  | `1`  | Direct Message       | `DM`                    _A direct message between users_                               |
  | `2`  | Guild Voice          | `GUILD_VOICE`           _A voice channel within a server_                              |
  | `3`  | Group Direct Message | `GROUP_DM`              _A direct message between multiple users_                      |
  | `4`  | Guild Category       | `GUILD_CATEGORY`        _A category that contains up to 50 channels_                   |
  | `5`  | Guild Announcements  | `GUILD_NEWS`            _A channel that users can follow and crosspost from_           |
  | `6`  | Guild Store          | `GUILD_STORE`           _A channel in which developers can sell their game on Discord_ |
  | `10` | Guild News Thread    | `GUILD_NEWS_THREAD`     _A temporary sub-channel within a `5` channel_                 |
  | `11` | Guild Public Thread  | `GUILD_PUBLIC_THREAD`   _A temporary sub-channel within an `0` channel_                |
  | `12` | Guild Private Thread | `GUILD_PRIVATE_THREAD`  _A temporary private sub-channel within an `0`                 |
  | `13` | Guild Stage Voice    | `GUILD_STAGE_VOICE`     _A voice channel for hosting events with an audience_          |

  More information about _Discord Channel Types_ can be found on the [Discord API Channel Type Documentation](https://discord.com/developers/docs/resources/channel#channel-object-channel-types).
  """
  ## Unsure if it should be here or at the top of the documentation, at both for now.
  @type type :: integer()

  @typedoc """
  The id of the guild the channel is located in.
  """
  @type guild_id :: Guild.id()

  @typedoc """
  The position of the channel in the sidebar of the guild.
  """
  @type position :: integer()

  @typedoc """
  A list of permission overwrites applied to the channel.
  """
  @type permission_overwrites :: [Overwrite.t()]

  @typedoc """
  The name of the channel.
  """
  @type name :: String.t()

  @typedoc """
  The topic of the channel.
  """
  @type topic :: String.t()

  @typedoc """
  The channels NSFW setting.
  """
  @type nsfw :: boolean()

  @typedoc """
  The id of the last message sent in the channel.
  """
  @type last_message_id :: Message.id() | nil

  @typedoc """
  The bitate of the channel.
  """
  @type bitrate :: integer()

  @typedoc """
  The users rate limit.

  Amount of seconds a user has to wait before sending another message (0-21600); bots, as well as users with the permission manage_messages or manage_channel, are unaffected
  """
  @typedoc since: "0.5"
  @type rate_limit_per_user :: String.t() | nil

  @typedoc """
  The user limit of a voice channel.
  """
  @type user_limit :: integer()

  @typedoc """
  A list of users in a group channel.
  """
  @type recipients :: [User.t()]

  @typedoc """
  The hash of the channels icon.
  """
  @type icon :: String.t() | nil

  @typedoc """
  The id of the channels creator.
  """
  @type owner_id :: User.id()

  @typedoc """
  The id of the application that created the channel.
  """
  @type application_id :: Snowflake.t() | nil

  @typedoc """
  The id of the category channel that this channel is located under.
  """
  @type parent_id :: Channel.id() | nil

  @typedoc """
  Timestamp for the last pinned message.
  """
  @type last_pin_timestamp :: String.t() | nil

  @typedoc """
  Region id for the voice channel.

  More information about _region ids_ can be found on the [Discord API Voice Region Object Documentation](https://discord.com/developers/docs/resources/voice#voice-region-object).
  """
  @typedoc since: "0.5"
  @type rtc_region :: String.t() | nil

  @typedoc """
  The video quality mode of the voice channel.

  More information about _video quality modes_ can be found on the [Discord API Video Quality Mode Documentation](https://discord.com/developers/docs/resources/channel#channel-object-video-quality-modes).
  """
  @typedoc since: "0.5"
  @type video_quality_mode :: integer() | nil

  @typedoc """
  Approximate count of messages in a thread, capped at 50.
  """
  @typedoc since: "0.5"
  @type message_count :: integer() | nil

  @typedoc """
  Approximate count of members in a thread, capped at 50.
  """
  @typedoc since: "0.5"
  @type member_count :: integer() | nil

  @typedoc """
  The video quality mode of the voice channel.

  More information about _video quality modes_ can be found on the [Discord API Video Quality Mode Documentation](https://discord.com/developers/docs/resources/channel#channel-object-video-quality-modes).
  """
  @typedoc since: "0.5"
  @type thread_metadata :: %{
          archived: archived,
          auto_archive_duration: auto_archive_duration,
          archive_timestamp: archive_timestamp,
          locked: boolean()
        }

  @typedoc """
  When the thread was archived.
  """
  @typedoc since: "0.5"
  @type archive_timestamp :: String.t() | nil

  @typedoc """
  The threads locked status.
  """
  @typedoc since: "0.5"
  @type locked :: boolean()

  @typedoc """
  The threads archived status.
  """
  @typedoc since: "0.5"
  @type archived :: boolean()

  @typedoc """
  Archive duration for the thread.

  - 60, 1 hour
  - 1440, 24 hours
  - 4320, 3 days
  - 10080, 7 days
  """
  @typedoc since: "0.5"
  @type auto_archive_duration :: integer()

  @typedoc """
  Indicates whether a user has joined a thread or not.
  """
  @typedoc since: "0.5"
  @type member :: %{
          id: id,
          user_id: user_id,
          join_timestamp: join_timestamp,
          flags: flags
        }
  @typedoc """
  User id of the threads creator.
  """
  @typedoc since: "0.5"
  @type user_id :: Snowflake.t()

  @typedoc """
  When the current user joined the thread.
  """
  @typedoc since: "0.5"
  @type join_timestamp :: String.t()

  @typedoc """
  User thread settings, currently only used for notifications.
  """
  @typedoc since: "0.5"
  @type flags :: integer()

  @typedoc """
  Default duration for newly created threads.

  - 60, 1 hour
  - 1440, 24 hours
  - 4320, 3 days
  - 10080, 7 days
  """
  @typedoc since: "0.5"
  @type default_auto_archive_duration :: integer()

  @typedoc """
  Computed permissions of the invoking user.t()

  computed permissions for the invoking user in the channel, including overwrites, only included when part of the resolved data received on a slash command interaction
  """
  @typedoc since: "0.5"
  @type permissions :: String.t()

  @typedoc """
  Type 0 partial channel object representing a text channel within a guild.
  """
  @type guild_text_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          name: name,
          type: type,
          position: position,
          permission_overwrites: permission_overwrites,
          rate_limit_per_user: rate_limit_per_user,
          nsfw: nsfw,
          topic: topic,
          last_message_id: last_message_id,
          parent_id: parent_id,
          default_auto_archive_duration: default_auto_archive_duration
        }

  @typedoc """
  Type 1 partial channel object representing a direct message.
  """
  @type dm_channel :: %__MODULE__{
          id: id,
          type: 1,
          last_message_id: last_message_id,
          recipients: recipients,
          last_pin_timestamp: last_pin_timestamp
        }

  @typedoc """
  Type 2 partial channel object representing an audio channel within a guild.
  """
  @type guild_voice_channel :: %__MODULE__{
          id: id,
          type: 2,
          guild_id: guild_id,
          position: position,
          permission_overwrites: permission_overwrites,
          name: name,
          nsfw: nsfw,
          bitrate: bitrate,
          user_limit: user_limit,
          parent_id: parent_id,
          rtc_region: rtc_region
        }

  @typedoc """
  Type 3 partial channel object representing a group direct message.
  """
  @type group_dm_channel :: %__MODULE__{
          id: id,
          type: 3,
          name: name,
          last_message_id: last_message_id,
          recipients: recipients,
          icon: icon,
          owner_id: owner_id,
          application_id: application_id
        }

  @typedoc """
  Type 4 partial channel object representing a channel category.

  > Note:  Other channels `parent_id` field refers to this type of object.
  """

  @type guild_category_channel :: %__MODULE__{
          id: id,
          type: 4,
          guild_id: guild_id,
          position: position,
          permission_overwrites: permission_overwrites,
          name: name,
          nsfw: nsfw,
          parent_id: parent_id
        }

  @typedoc """
  Type 5 partial channel object representing a news channel.
  """
  @typedoc since: "0.5"
  @type guild_news_channel :: %__MODULE__{
          id: id,
          type: 5,
          guild_id: guild_id,
          name: name,
          position: position,
          permission_overwrites: permission_overwrites,
          nsfw: nsfw,
          topic: topic,
          last_message_id: last_message_id,
          parent_id: parent_id,
          default_auto_archive_duration: default_auto_archive_duration
        }

  @typedoc """
  Type 6 partial channel object representing a store channel.
  """
  @typedoc since: "0.5"
  @type guild_store_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          name: name,
          type: type,
          position: position,
          permission_overwrites: permission_overwrites,
          nsfw: nsfw,
          parent_id: parent_id
        }

  @typedoc """
  Type 10 partial channel object representing a news thread.
  """
  @typedoc since: "0.5"
  @type guild_news_thread_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          parent_id: parent_id,
          owner_id: owner_id,
          name: name,
          type: type,
          last_message_id: last_message_id,
          message_count: message_count,
          member_count: member_count,
          rate_limit_per_user: rate_limit_per_user,
          thread_metadata: thread_metadata
        }

  @typedoc """
  Type 11 partial channel object representing a standard thread.
  """
  @typedoc since: "0.5"
  @type guild_public_thread_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          parent_id: parent_id,
          owner_id: owner_id,
          name: name,
          type: type,
          last_message_id: last_message_id,
          message_count: message_count,
          member_count: member_count,
          rate_limit_per_user: rate_limit_per_user,
          thread_metadata: thread_metadata
        }

  @typedoc """
  Type 12 partial channel object representing a private thread.
  """
  @typedoc since: "0.5"
  @type guild_private_thread_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          parent_id: parent_id,
          owner_id: owner_id,
          name: name,
          type: type,
          last_message_id: last_message_id,
          message_count: message_count,
          member_count: member_count,
          rate_limit_per_user: rate_limit_per_user,
          thread_metadata: thread_metadata
        }

  @typedoc """
  Type 13 channel object representing a stage channel.
  """
  @typedoc since: "0.5"
  @type guild_stage_voice_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          parent_id: parent_id,
          owner_id: owner_id,
          name: name,
          type: type,
          last_message_id: last_message_id,
          message_count: message_count,
          member_count: member_count,
          rate_limit_per_user: rate_limit_per_user,
          thread_metadata: thread_metadata
        }

  @typedoc """
  A partial channel object representing a channel mention.

  More information about the _Discord Channel Mention Object_ can be found at the [Discord API Channel Mention Object
  Documentation](https://discord.com/developers/docs/resources/channel#channel-mention-object).
  """
  @type channel_mention :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          type: type,
          name: name
        }

  @typedoc """
  Guild channel types
  """
  @typedoc deprecated: "See t.0"
  @type guild_channel ::
          guild_text_channel
          | guild_voice_channel
          | guild_category_channel

  @typedoc """
  All valid text channels.
  """
  @typedoc deprecated: "See t.0"
  @type text_channel ::
          guild_text_channel
          | dm_channel
          | group_dm_channel

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a voice channel

  """
  @typedoc deprecated: "See t.0"
  @type voice_channel :: guild_voice_channel

  @typedoc """
  All valid channel types.
  """
  @type t ::
          guild_text_channel
          | dm_channel
          | guild_voice_channel
          | group_dm_channel
          | guild_category_channel
          | guild_news_channel
          | guild_store_channel
          | guild_news_thread_channel
          | guild_public_thread_channel
          | guild_private_thread_channel
          | guild_stage_voice_channel

  @doc """
  Convert a channel into a mention.

  Handles the conversion of a `Nostrum.Struct.Channel` into the required format to _mention_ the channel within a message.

  ## Parameters

    - channel: `t:Nostrum.Struct.Channel.t/0`

  ## Examples

      iex> Nostrum.Cache.ChannelCache.get(381889573426429952)
      ...> |> Nostrum.Struct.Channel.mention()
      "<#381889573426429952>"

      iex> Nostrum.GuildCache.get(81384788765712384)
      ...> |> Nostrum.Struct.Channel.mention()
      "<#Invalid Channel>"

  """
  @spec mention(t) :: String.t()
  def mention(channel)
  def mention(%__MODULE__{id: id}), do: "<##{id}>"
  def mention(_any), do: "<#Invalid Channel>"

  @doc """
  Convert a channel into a hyperlink.

  Handles the conversion of a `Nostrum.Struct.Channel` into a [link](#link/1) to that channel.

  > Note: if a user does not already have access to that channel, they will receive "Resource not available" upon following the hyperlink. If you do not know if they have access to the channel or want to grant temporary access it might be safer to use `Nostrum.Api.create_channel_invite/3`

  ## Parameters

  - channel: `t:Nostrum.Struct.Channel.t/0`

  ## Examples

      iex> Nostrum.Cache.ChannelCache.get(381889573426429952)
      ...> |> Nostrum.Struct.Channel.link()
      "https://discordapp.com/channels/#81384788765712384/381889573426429952"

      iex> Nostrum.GuildCache.get(81384788765712384)
      ...> |> Nostrum.Struct.Channel.link()
      "Channel Not Found"

  """
  @spec link(t) :: String.t()
  def link(channel)

  def link(%__MODULE__{id: id, guild_id: guild_id}),
    do: "https://discordapp.com/channels/#{guild_id}/#{id}"

  def link(_any), do: "Channel Not Found"

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:permission_overwrites, nil, &Util.cast(&1, {:list, {:struct, Overwrite}}))
      |> Map.update(:last_message_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:recipients, nil, &Util.cast(&1, {:list, {:struct, User}}))
      |> Map.update(:owner_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:application_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:parent_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
