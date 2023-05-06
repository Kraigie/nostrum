defmodule Nostrum.Struct.Guild do
  # fields that are only sent on GUILD_CREATE
  @guild_create_fields [
    :joined_at,
    :large,
    :unavailable,
    :member_count,
    :voice_states,
    :channels,
    :guild_scheduled_events,
    :threads
  ]

  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Nostrum.Struct.{Channel, Emoji, User}
  alias Nostrum.Struct.Guild.{Role, ScheduledEvent}
  alias Nostrum.{Constants, Snowflake, Util}

  defstruct [
    :id,
    :name,
    :icon,
    :splash,
    :owner_id,
    :region,
    :afk_channel_id,
    :afk_timeout,
    :verification_level,
    :default_message_notifications,
    :explicit_content_filter,
    :roles,
    :emojis,
    :features,
    :mfa_level,
    :application_id,
    :widget_enabled,
    :widget_channel_id,
    :system_channel_id,
    :rules_channel_id,
    :public_updates_channel_id,
    :joined_at,
    :large,
    :unavailable,
    :member_count,
    :voice_states,
    :channels,
    :guild_scheduled_events,
    :vanity_url_code,
    :threads
  ]

  @typedoc "The guild's id"
  @type id :: Snowflake.t()

  @typedoc "The name of the guild."
  @type name :: String.t()

  @typedoc "The hash of the guild's icon"
  @type icon :: String.t() | nil

  @typedoc "The hash of the guild's splash"
  @type splash :: String.t() | nil

  @typedoc "The id of the guild owner"
  @type owner_id :: User.id()

  @typedoc "The id of the voice region"
  @type region :: String.t()

  @typedoc "The id of the guild's afk channel"
  @type afk_channel_id :: Channel.id() | nil

  @typedoc "The time someone must be afk before being moved"
  @type afk_timeout :: integer

  @typedoc "The level of verification"
  @type verification_level :: integer

  @typedoc """
  Default message notifications level.
  """
  @type default_message_notifications :: integer

  @typedoc """
  Explicit content filter level.
  """
  @type explicit_content_filter :: integer

  @typedoc "List of roles"
  @type roles :: %{required(Role.id()) => Role.t()}

  @typedoc "List of emojis"
  @type emojis :: [Emoji.t()]

  @typedoc "List of guild features"
  @type features :: [String.t()]

  @typedoc "Required MFA level of the guild"
  @type mfa_level :: integer

  @typedoc """
  Application id of the guild creator if it is bot created.
  """
  @type application_id :: Snowflake.t() | nil

  @typedoc """
  Whether or not the server widget is enabled.
  """
  @type widget_enabled :: boolean | nil

  @typedoc """
  The channel id for the server widget.
  """
  @type widget_channel_id :: Channel.id()

  @typedoc """
  The id of the channel to which system messages are sent.
  """
  @type system_channel_id :: Channel.id() | nil

  @typedoc """
  The id of the channel that is used for rules. This is only available to guilds that
  contain ``PUBLIC`` in `t:features/0`.
  """
  @type rules_channel_id :: Channel.id() | nil

  @typedoc """
  The id of the channel where admins and moderators receive notices from Discord. This
  is only available to guilds that contain ``PUBLIC`` in `t:features/0`.
  """
  @type public_updates_channel_id :: Channel.id() | nil

  @typedoc "Date the bot user joined the guild"
  @type joined_at :: String.t() | nil

  @typedoc "Whether the guild is considered 'large'"
  @type large :: boolean | nil

  @typedoc "Whether the guild is available"
  @type unavailable :: boolean | nil

  @typedoc "Total number of members in the guild"
  @type member_count :: integer | nil

  @typedoc "List of voice states as maps"
  @type voice_states :: list(map) | nil

  @typedoc "List of channels"
  @type channels :: %{required(Channel.id()) => Channel.t()} | nil

  @typedoc "List of scheduled events"
  @type guild_scheduled_events :: list(ScheduledEvent.t()) | nil

  @typedoc "Guild invite vanity URL"
  @type vanity_url_code :: String.t() | nil

  @typedoc "All active threads in the guild that the current user has permission to view"
  @typedoc since: "0.5.1"
  @type threads :: %{required(Channel.id()) => Channel.t()} | nil

  @typedoc """
  A `Nostrum.Struct.Guild` that is sent on user-specific rest endpoints.
  """
  @type user_guild :: %__MODULE__{
          id: id,
          name: name,
          icon: icon,
          splash: nil,
          owner_id: nil,
          region: nil,
          afk_channel_id: nil,
          afk_timeout: nil,
          verification_level: nil,
          default_message_notifications: nil,
          explicit_content_filter: nil,
          roles: nil,
          emojis: nil,
          features: nil,
          mfa_level: nil,
          application_id: nil,
          widget_enabled: nil,
          widget_channel_id: nil,
          system_channel_id: nil,
          rules_channel_id: nil,
          public_updates_channel_id: nil,
          joined_at: nil,
          large: nil,
          unavailable: nil,
          member_count: nil,
          voice_states: nil,
          channels: nil,
          vanity_url_code: nil,
          threads: nil
        }

  @typedoc """
  A `Nostrum.Struct.Guild` that is sent on guild-specific rest endpoints.
  """
  @type rest_guild :: %__MODULE__{
          id: id,
          name: name,
          icon: icon,
          splash: splash,
          owner_id: owner_id,
          region: region,
          afk_channel_id: afk_channel_id,
          afk_timeout: afk_timeout,
          verification_level: verification_level,
          default_message_notifications: default_message_notifications,
          explicit_content_filter: explicit_content_filter,
          roles: roles,
          emojis: emojis,
          features: features,
          mfa_level: mfa_level,
          application_id: application_id,
          widget_enabled: widget_enabled,
          widget_channel_id: widget_channel_id,
          system_channel_id: system_channel_id,
          rules_channel_id: rules_channel_id,
          public_updates_channel_id: public_updates_channel_id,
          vanity_url_code: vanity_url_code,
          joined_at: nil,
          large: nil,
          unavailable: nil,
          member_count: nil,
          voice_states: nil,
          channels: nil,
          guild_scheduled_events: nil,
          threads: nil
        }

  @typedoc """
  A `Nostrum.Struct.Guild` that is unavailable.
  """
  @type unavailable_guild :: %__MODULE__{
          id: id,
          name: nil,
          icon: nil,
          splash: nil,
          owner_id: nil,
          region: nil,
          afk_channel_id: nil,
          afk_timeout: nil,
          verification_level: nil,
          default_message_notifications: nil,
          explicit_content_filter: nil,
          roles: nil,
          emojis: nil,
          features: nil,
          mfa_level: nil,
          application_id: nil,
          widget_enabled: nil,
          widget_channel_id: nil,
          system_channel_id: nil,
          rules_channel_id: nil,
          public_updates_channel_id: nil,
          joined_at: nil,
          large: nil,
          unavailable: true,
          member_count: nil,
          voice_states: nil,
          channels: nil,
          guild_scheduled_events: nil,
          vanity_url_code: nil,
          threads: nil
        }

  @typedoc """
  A `Nostrum.Struct.Guild` that is fully available.
  """
  @type available_guild :: %__MODULE__{
          id: id,
          name: name,
          icon: icon,
          splash: splash,
          owner_id: owner_id,
          region: region,
          afk_channel_id: afk_channel_id,
          afk_timeout: afk_timeout,
          verification_level: verification_level,
          default_message_notifications: default_message_notifications,
          explicit_content_filter: explicit_content_filter,
          roles: roles,
          emojis: emojis,
          features: features,
          mfa_level: mfa_level,
          application_id: application_id,
          widget_enabled: widget_enabled,
          widget_channel_id: widget_channel_id,
          system_channel_id: system_channel_id,
          rules_channel_id: rules_channel_id,
          public_updates_channel_id: public_updates_channel_id,
          joined_at: joined_at,
          large: large,
          unavailable: false,
          member_count: member_count,
          voice_states: voice_states,
          channels: channels,
          guild_scheduled_events: guild_scheduled_events,
          vanity_url_code: vanity_url_code,
          threads: threads
        }

  @type t ::
          available_guild
          | unavailable_guild
          | rest_guild
          | user_guild

  @doc ~S"""
  Returns the URL of a guild's icon, or `nil` if there is no icon.

  Supported image formats are PNG, JPEG, and WebP.

  ## Examples

  ```elixir
  iex> guild = %Nostrum.Struct.Guild{icon: "86e39f7ae3307e811784e2ffd11a7310",
  ...>                               id: 41771983423143937}
  iex> Nostrum.Struct.Guild.icon_url(guild)
  "https://cdn.discordapp.com/icons/41771983423143937/86e39f7ae3307e811784e2ffd11a7310.webp"
  iex> Nostrum.Struct.Guild.icon_url(guild, "png")
  "https://cdn.discordapp.com/icons/41771983423143937/86e39f7ae3307e811784e2ffd11a7310.png"

  iex> guild = %Nostrum.Struct.Guild{icon: nil}
  iex> Nostrum.Struct.Guild.icon_url(guild)
  nil
  ```
  """
  @spec icon_url(t, String.t()) :: String.t() | nil
  def icon_url(guild, image_format \\ "webp")
  def icon_url(%__MODULE__{icon: nil}, _), do: nil

  def icon_url(%__MODULE__{icon: icon, id: id}, image_format),
    do: URI.encode(Constants.cdn_url() <> Constants.cdn_icon(id, icon, image_format))

  @doc ~S"""
  Returns the URL of a guild's splash, or `nil` if there is no splash.

  Supported image formats are PNG, JPEG, and WebP.

  ## Examples

  ```elixir
  iex> guild = %Nostrum.Struct.Guild{splash: "86e39f7ae3307e811784e2ffd11a7310",
  ...>                               id: 41771983423143937}
  iex> Nostrum.Struct.Guild.splash_url(guild)
  "https://cdn.discordapp.com/splashes/41771983423143937/86e39f7ae3307e811784e2ffd11a7310.webp"
  iex> Nostrum.Struct.Guild.splash_url(guild, "png")
  "https://cdn.discordapp.com/splashes/41771983423143937/86e39f7ae3307e811784e2ffd11a7310.png"

  iex> guild = %Nostrum.Struct.Guild{splash: nil}
  iex> Nostrum.Struct.Guild.splash_url(guild)
  nil
  ```
  """
  @spec splash_url(t, String.t()) :: String.t() | nil
  def splash_url(guild, image_format \\ "webp")
  def splash_url(%__MODULE__{splash: nil}, _), do: nil

  def splash_url(%__MODULE__{splash: splash, id: id}, image_format),
    do: URI.encode(Constants.cdn_url() <> Constants.cdn_splash(id, splash, image_format))

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:owner_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:afk_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:roles, nil, &Util.cast(&1, {:index, [:id], {:struct, Role}}))
      |> Map.update(:emojis, nil, &Util.cast(&1, {:list, {:struct, Emoji}}))
      |> Map.update(:application_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:widget_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:system_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:rules_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:public_updates_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channels, nil, &Util.cast(&1, {:index, [:id], {:struct, Channel}}))
      |> Map.update(:joined_at, nil, &Util.maybe_to_datetime/1)
      |> Map.update(
        :guild_scheduled_events,
        nil,
        &Util.cast(&1, {:list, {:struct, ScheduledEvent}})
      )
      |> Map.update(:threads, nil, &Util.cast(&1, {:index, [:id], {:struct, Channel}}))

    struct(__MODULE__, new)
  end

  @doc false
  @spec merge(t, t) :: t
  def merge(old_guild, new_guild) do
    Map.merge(old_guild, new_guild, &handle_key_conflict/3)
  end

  # Make it so that values which are only sent on GUILD_CREATE are not replaced with nil
  defp handle_key_conflict(key, old, nil) when key in @guild_create_fields do
    old
  end

  defp handle_key_conflict(_key, _old, new) do
    new
  end
end
