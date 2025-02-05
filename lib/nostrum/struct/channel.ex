defmodule Nostrum.Struct.Channel do
  @moduledoc """
  Struct and helper functions for working with channels.

  ## Channel Struct

  The channel struct is used by Nostrum to represent a _Discord Channel Object_. More information can be found on the [Discord API Channel Documentation](https://discord.com/developers/docs/resources/channel#channels-resource).

  The struct can have one of several forms depending on the type of channel. You can read more about the individual channel types [below](#module-channel-types).

  A typical channel would appear as:

  ```elixir
  %Nostrum.Struct.Channel{
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

  ## Channel Caching

  Channels are cached within the guild object they are a member of. To fetch a cached channel you should use the `Nostrum.Cache.ChannelGuildMapping` cache to map a cache to a guild ID.

  Once you have a guild ID, you can use the `Nostrum.Cache.GuildCache` to fetch the guild and all channels, then use the `t:Nostrum.Struct.Guild.channels/0` field to find the channel.

  > #### Note on DM Channels {: .tip}
  >
  > The `Nostrum.Cache.ChannelGuildMapping` by design only caches a mapping of channels to their respective guilds, and the stored channel object sits within the `Nostrum.Cache.GuildCache`.
  >
  > Due to how Direct Message channels work (no `CHANNEL_CREATE` events, not sent in any gateway events, etc.), we make no attempt to store direct message channels within any of the
  > built-in Nostrum caching adapters.
  >
  > If you wish to implement this functionality yourself, you will have to fetch the channel details from the API by looking at the `channel_id` field of received message objects,
  > you can then store these details in your own choice of caching system (ETS, Mnesia, etc.).

  ### Example

  ```elixir
  > guild_id = Nostrum.Cache.ChannelGuildMapping.get(1229955694258684005)
  1226944827137069107
  > {:ok, guild} = Nostrum.Cache.GuildCache.get(guild_id)
  {:ok,
    %Nostrum.Struct.Guild{
      id: 1226944827137069107,
      name: "Craig Cat Zone",
      ...
  }}
  > guild.channels[channel_id].name
  "cat-general"
  ```

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

    ![Discord Channels](./assets/channel_types.png)

    The currently implemented channel types are:

  |     |Channel Type                                                 |                                                                  |
  |---- |--------------------                                         |---------------------------------------------------------------   |
  |`0`  |[`GUILD_TEXT`](`t:guild_text_channel/0`)                     |_A text channel within a server_                                  |
  |`1`  |[`DM`](`t:dm_channel/0`)                                     |_A direct message between users_                                  |
  |`2`  |[`GUILD_VOICE`](`t:guild_voice_channel/0`)                   |_A voice channel within a server_                                 |
  |`3`  |[`GROUP_DM`](`t:group_dm_channel/0`)                         |_A direct message between multiple users_                         |
  |`4`  |[`GUILD_CATEGORY`](`t:guild_category_channel/0`)             |_A category that contains up to 50 channels_                      |
  |`5`  |[`GUILD_NEWS`](`t:guild_news_channel/0`)                     |_A channel that users can follow and crosspost_                   |
  |`6`  |[`GUILD_STORE`](`t:guild_store_channel/0`)                   |_A channel to sell games on Discord_                              |
  |`10` |[`GUILD_NEWS_THREAD`](`t:guild_news_thread_channel/0`)       |_A temporary sub-channel within a news channel_                   |
  |`11` |[`GUILD_PUBLIC_THREAD`](`t:guild_public_thread_channel/0`)   |_A temporary sub-channel within a text channel_                   |
  |`12` |[`GUILD_PRIVATE_THREAD`](`t:guild_private_thread_channel/0`) |_A temporary private sub-channel within a text channel_           |
  |`13` |[`GUILD_STAGE_VOICE`](`t:guild_stage_voice_channel/0`)       |_A voice channel for hosting events with an audience_             |
  |`15` |[`GUILD_FORUM`](`t:guild_forum_channel/0`)                   |_A channel that can only contain threads                          |

  You can use one of the `Nostrum.Constants.ChannelType` methods.

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
    :permissions,
    :newly_created,
    :available_tags,
    :applied_tags,
    :default_reaction_emoji,
    :default_thread_rate_limit_per_user,
    :default_sort_order,
    :default_forum_layout
  ]

  @typedoc """
  The id of the channel object.
  """
  @type id :: Snowflake.t()

  @typedoc """
  The type of channel.

  More information about _Discord Channel Types_ can be found under the [`types`](#module-channel-types) on the [Discord API Channel Type Documentation](https://discord.com/developers/docs/resources/channel#channel-object-channel-types).
  """
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
  Whether the NSFW setting is enabled for this channel.
  """
  @type nsfw :: boolean()

  @typedoc """
  The id of the last message sent in the channel.

  For `GUILD_FORUM` channels, this is the last thread created in the channel.
  """
  @type last_message_id :: Message.id() | nil

  @typedoc """
  The bitate of the voice channel.
  """
  @type bitrate :: integer()

  @typedoc """
  The users rate limit.

  Amount of seconds a user has to wait before sending another message (0-21600); bots, as well as users with the permission manage_messages or manage_channel, are unaffected
  """
  @typedoc since: "0.5.0"
  @type rate_limit_per_user :: integer() | nil

  @typedoc """
  The `:rate_limit_per_user` which will be applied to threads created in the channel, in seconds.
  """
  @typedoc since: "0.7.0"
  @type default_thread_rate_limit_per_user :: integer() | nil

  @typedoc """
  The default sort order for posts in a `GUILD_FORUM` channel.
  """
  @typedoc since: "0.10.1"
  @type default_sort_order :: integer() | nil

  @typedoc """
  The default layout for posts in a `GUILD_FORUM` channel.
  """
  @typedoc since: "0.10.1"
  @type default_forum_layout :: integer() | nil

  @typedoc """
  The user limit of a voice channel.
  """
  @type user_limit :: integer()

  @typedoc """
  A list of users in a group DM.
  """
  @type recipients :: [User.t()]

  @typedoc """
  The hash of the channels icon.
  """
  @type icon :: String.t() | nil

  @typedoc """
  The id of the user of a group direct message or thread.

  This applies to user created channels.
  """
  @type owner_id :: User.id()

  @typedoc """
  The id of the application that created a group direct message or thread.

  This applies to bot created channels.
  """
  @type application_id :: Snowflake.t() | nil

  @typedoc """
  The id of the parent channel that this channel is located under.

  For threads, that is the channel that contains the thread. For regular channels, it is the category that the channel is located under.
  """
  @type parent_id :: Channel.id() | nil

  @typedoc """
  Timestamp for the last pinned message.
  """
  @type last_pin_timestamp :: DateTime.t() | nil

  @typedoc """
  Region id for the channel.

  More information about _region ids_ can be found on the [Discord API Voice Region Object Documentation](https://discord.com/developers/docs/resources/voice#voice-region-object).
  """
  @typedoc since: "0.5.0"
  @type rtc_region :: String.t() | nil

  @typedoc """
  The video quality mode of the channel.

  More information about _video quality modes_ can be found on the [Discord API Video Quality Mode Documentation](https://discord.com/developers/docs/resources/channel#channel-object-video-quality-modes).
  """
  @typedoc since: "0.5.0"
  @type video_quality_mode :: integer() | nil

  @typedoc """
  Approximate count of messages in a thread, capped at 50.
  """
  @typedoc since: "0.5.0"
  @type message_count :: integer() | nil

  @typedoc """
  Approximate count of members in a thread, capped at 50.
  """
  @typedoc since: "0.5.0"
  @type member_count :: integer() | nil

  @typedoc """
  Thread-specific fields not needed by other channels.
  """
  @typedoc since: "0.5.0"
  @type thread_metadata :: %{
          archived: archived,
          auto_archive_duration: auto_archive_duration,
          archive_timestamp: archive_timestamp,
          locked: boolean(),
          invitable: boolean() | nil,
          create_timestamp: DateTime.t() | nil
        }

  @typedoc """
  When the thread was archived.
  """
  @typedoc since: "0.5.0"
  @type archive_timestamp :: DateTime.t() | nil

  @typedoc """
  The threads locked status.
  """
  @typedoc since: "0.5.0"
  @type locked :: boolean()

  @typedoc """
  The threads archived status.
  """
  @typedoc since: "0.5.0"
  @type archived :: boolean()

  @typedoc """
  Archive duration for the thread in minutes.

  - 60, 1 hour
  - 1440, 24 hours
  - 4320, 3 days
  - 10080, 7 days
  """
  @typedoc since: "0.5.0"
  @type auto_archive_duration :: integer()

  @typedoc """
  Present when the bot joins a thread.

  Note: This is omitted on threads that the bot can immediately access on `:GUILD_CREATE` events received.
  """
  @typedoc since: "0.5.0"
  @type member :: %{
          id: id,
          user_id: user_id,
          join_timestamp: join_timestamp,
          flags: flags
        }

  @typedoc """
  User id of the threads creator.
  """
  @typedoc since: "0.5.0"
  @type user_id :: Snowflake.t() | nil

  @typedoc """
  When the user joined the thread.
  """
  @typedoc since: "0.5.0"
  @type join_timestamp :: DateTime.t()

  @typedoc """
  User thread settings, currently only used for notifications.
  """
  @typedoc since: "0.5.0"
  @type flags :: integer()

  @typedoc """
  Default duration for newly created threads in minutes.

  - 60, 1 hour
  - 1440, 24 hours
  - 4320, 3 days
  - 10080, 7 days
  """
  @typedoc since: "0.5.0"
  @type default_auto_archive_duration :: integer()

  @typedoc """
  Computed permissions of the invoking user.

  Permissions for the invoking user in the channel, including overwrites, only included when part of the resolved data received on a slash command interaction
  """
  @typedoc since: "0.5.0"
  @type permissions :: String.t()

  @typedoc """
  Included only in the `THREAD_CREATE` event.
  """
  @typedoc since: "0.5.1"
  @type newly_created :: boolean | nil

  @typedoc """
  A map representing a tag for use in forum channels.

  `:moderated` indicates whether the tag can only be added or removed by moderators.
  `:emoji_id` and `:emoji_name` are mutually exclusive and indicate the emoji used to represent the tag.
  """
  @typedoc since: "0.7.0"
  @type forum_tag :: %{
          id: Snowflake.t(),
          name: String.t(),
          moderated: boolean(),
          emoji_id: Snowflake.t() | nil,
          emoji_name: String.t() | nil
        }

  @typedoc since: "0.7.0"
  @type applied_tags :: [Snowflake.t()] | nil

  @typedoc """
  An object that specifies the emoji to use as the default way to react to a forum post.

  `:emoji_id` and `:emoji_name` are mutually exclusive
  """
  @typedoc since: "0.7.0"
  @type default_reaction_emoji ::
          %{
            emoji_id: Snowflake.t() | nil,
            emoji_name: String.t() | nil
          }
          | nil

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
  @typedoc since: "0.5.0"
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
  @typedoc since: "0.5.0"
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
  @typedoc since: "0.5.0"
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
          thread_metadata: thread_metadata,
          newly_created: newly_created
        }

  @typedoc """
  Type 11 partial channel object representing a standard thread.
  """
  @typedoc since: "0.5.0"
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
          thread_metadata: thread_metadata,
          newly_created: newly_created,
          applied_tags: applied_tags()
        }

  @typedoc """
  Type 12 partial channel object representing a private thread.
  """
  @typedoc since: "0.5.0"
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
          thread_metadata: thread_metadata,
          newly_created: newly_created
        }

  @typedoc """
  Type 13 channel object representing a stage channel.
  """
  @typedoc since: "0.5.0"
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
  Type 15 a guild forum channel.
  """
  @typedoc since: "0.7.0"
  @type guild_forum_channel :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          name: name,
          type: type,
          position: position,
          permission_overwrites: permission_overwrites,
          nsfw: nsfw,
          parent_id: parent_id,
          last_message_id: last_message_id,
          available_tags: [forum_tag],
          rate_limit_per_user: rate_limit_per_user,
          default_reaction_emoji: default_reaction_emoji,
          default_thread_rate_limit_per_user: default_thread_rate_limit_per_user,
          default_sort_order: default_sort_order,
          default_forum_layout: default_forum_layout
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
  @typedoc deprecated: "See t:guild_voice_channel/0"
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
          | guild_forum_channel

  @doc """
  Convert a channel into a mention.

  Handles the conversion of a `Nostrum.Struct.Channel` into the required format to _mention_ the channel within a message. Mentioning the channel will provide a clickable link to take the user to the channel.

  ## Parameters

  - channel: `t:Nostrum.Struct.Channel.t/0`

  ## Examples

  ```elixir
  channel
  |> Nostrum.Struct.Channel.mention()
  "<#381889573426429952>"

  ```

  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{id: id}), do: "<##{id}>"

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
      |> Map.update(:last_pin_timestamp, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:archive_timestamp, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:join_timestamp, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:applied_tags, nil, &Util.cast(&1, {:list, Snowflake}))
      |> Map.update(:thread_metadata, nil, &cast_thread_metadata/1)
      |> Map.update(:available_tags, nil, &cast_available_tags/1)
      |> Map.update(:default_reaction_emoji, nil, &cast_default_reaction_emoji/1)

    struct(__MODULE__, new)
  end

  defp cast_thread_metadata(nil), do: nil

  defp cast_thread_metadata(map) do
    map
    |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
    |> Map.update(:archive_timestamp, nil, &Util.maybe_to_datetime/1)
    |> Map.update(:create_timestamp, nil, &Util.maybe_to_datetime/1)
  end

  defp cast_available_tags(nil), do: nil

  defp cast_available_tags(list) do
    Enum.map(list, fn map ->
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:emoji_id, nil, &Util.cast(&1, Snowflake))
    end)
  end

  defp cast_default_reaction_emoji(nil), do: nil

  defp cast_default_reaction_emoji(map) do
    map
    |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
    |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
  end
end
