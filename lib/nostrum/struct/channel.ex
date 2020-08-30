defmodule Nostrum.Struct.Channel do
  @moduledoc ~S"""
  Struct representing a Discord guild channel.

  A `Nostrum.Struct.Channel` represents all 5 types of channels. Each
  channel has a field `:type` with any of the following values:

    * `0` - GUILD_TEXT
    * `1` - DM
    * `2` - GUILD_VOICE
    * `3` - GROUP_DM
    * `4` - GUILD_CATEGORY

  More information can be found on the
  [Discord API Channel Documentation](https://discord.com/developers/docs/resources/channel#channel-object).

  ## Mentioning Channels in Messages

  A `Nostrum.Struct.Channel` can be mentioned in message content using the `String.Chars`
  protocol or `mention/1`.

  ```Elixir
  channel = %Nostrum.Struct.Channel{id: 381889573426429952}
  Nostrum.Api.create_message!(184046599834435585, "#{channel}")
  %Nostrum.Struct.Message{content: "<#381889573426429952>"}

  channel = %Nostrum.Struct.Channel{id: 280085880452939778}
  Nostrum.Api.create_message!(280085880452939778, "#{Nostrum.Struct.Channel.mention(channel)}")
  %Nostrum.Struct.Message{content: "<#280085880452939778>"}
  ```
  """

  alias Nostrum.Struct.{Overwrite, User}
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
    :recipients,
    :icon,
    :owner_id,
    :application_id,
    :parent_id,
    :last_pin_timestamp
  ]

  defimpl String.Chars do
    def to_string(channel), do: @for.mention(channel)
  end

  @typedoc "The channel's id"
  @type id :: Snowflake.t()

  @typedoc "The id of the channel's guild"
  @type guild_id :: Snowflake.t()

  @typedoc "The ordered position of the channel"
  @type position :: integer

  @typedoc "The list of overwrites"
  @type permission_overwrites :: [Overwrite.t()]

  @typedoc "The name of the channel"
  @type name :: String.t()

  @typedoc "Current channel topic"
  @type topic :: String.t()

  @typedoc "If the channel is nsfw"
  @type nsfw :: boolean

  @typedoc "Id of the last message sent"
  @type last_message_id :: Snowflake.t() | nil

  @typedoc "The bitrate of the voice channel"
  @type bitrate :: integer

  @typedoc "The user limit of the voice channel"
  @type user_limit :: integer

  @typedoc "The recipients of the DM"
  @type recipients :: [User.t()]

  @typedoc "The icon hash of the channel"
  @type icon :: String.t() | nil

  @typedoc "The id of the DM creator"
  @type owner_id :: Snowflake.t()

  @typedoc "The application id of the group DM creator if it is bot-created"
  @type application_id :: Snowflake.t() | nil

  @typedoc "The id of the parent category for a channel"
  @type parent_id :: Snowflake.t() | nil

  @typedoc "When the last pinned message was pinned"
  @type last_pin_timestamp :: String.t() | nil

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a text channel in a guild.
  """
  @type guild_text_channel :: %__MODULE__{
          id: id,
          type: 0,
          guild_id: guild_id,
          position: position,
          permission_overwrites: permission_overwrites,
          name: name,
          topic: topic,
          nsfw: nsfw,
          last_message_id: last_message_id,
          bitrate: nil,
          user_limit: nil,
          recipients: nil,
          icon: nil,
          owner_id: nil,
          application_id: nil,
          parent_id: parent_id,
          last_pin_timestamp: last_pin_timestamp
        }

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a DM channel.
  """
  @type dm_channel :: %__MODULE__{
          id: id,
          type: 1,
          guild_id: nil,
          position: nil,
          permission_overwrites: nil,
          name: nil,
          topic: nil,
          nsfw: nil,
          last_message_id: last_message_id,
          bitrate: nil,
          user_limit: nil,
          recipients: recipients,
          icon: nil,
          owner_id: nil,
          application_id: nil,
          parent_id: nil,
          last_pin_timestamp: nil
        }

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a voice channel in a guild.
  """
  @type guild_voice_channel :: %__MODULE__{
          id: id,
          type: 2,
          guild_id: guild_id,
          position: position,
          permission_overwrites: permission_overwrites,
          name: name,
          topic: nil,
          nsfw: nsfw,
          last_message_id: nil,
          bitrate: bitrate,
          user_limit: user_limit,
          recipients: nil,
          icon: nil,
          owner_id: nil,
          application_id: nil,
          parent_id: parent_id,
          last_pin_timestamp: nil
        }

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a group DM channel.
  """
  @type group_dm_channel :: %__MODULE__{
          id: id,
          type: 3,
          guild_id: nil,
          position: nil,
          permission_overwrites: nil,
          name: name,
          topic: nil,
          nsfw: nil,
          last_message_id: last_message_id,
          bitrate: nil,
          user_limit: nil,
          recipients: recipients,
          icon: icon,
          owner_id: owner_id,
          application_id: application_id,
          parent_id: nil,
          last_pin_timestamp: nil
        }

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a channel category in a guild.
  """
  @type channel_category :: %__MODULE__{
          id: id,
          type: 4,
          guild_id: guild_id,
          position: position,
          permission_overwrites: permission_overwrites,
          name: name,
          topic: nil,
          nsfw: nsfw,
          last_message_id: nil,
          bitrate: nil,
          user_limit: nil,
          recipients: nil,
          icon: nil,
          owner_id: nil,
          application_id: nil,
          parent_id: parent_id,
          last_pin_timestamp: nil
        }

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a channel in a guild.
  """
  @type guild_channel ::
          guild_text_channel
          | guild_voice_channel
          | channel_category

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a text channel.
  """
  @type text_channel ::
          guild_text_channel
          | dm_channel
          | group_dm_channel

  @typedoc """
  A `Nostrum.Struct.Channel` that represents a voice channel.
  """
  @type voice_channel :: guild_voice_channel

  @type t ::
          guild_text_channel
          | dm_channel
          | guild_voice_channel
          | group_dm_channel
          | channel_category

  @doc ~S"""
  Formats a `Nostrum.Struct.Channel` into a mention.

  ## Examples

  ```Elixir
  iex> channel = %Nostrum.Struct.Channel{id: 381889573426429952}
  ...> Nostrum.Struct.Channel.mention(channel)
  "<#381889573426429952>"
  ```
  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{id: id}), do: "<##{id}>"

  @doc false
  def p_encode do
    %__MODULE__{
      permission_overwrites: [Overwrite.p_encode()]
    }
  end

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
