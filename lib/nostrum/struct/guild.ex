defmodule Nostrum.Struct.Guild do
  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util

  defstruct [
    :id,
    :name,
    :icon,
    :splash,
    :owner_id,
    :region,
    :afk_channel_id,
    :afk_timeout,
    :embed_enabled,
    :embed_channel_id,
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
    :joined_at,
    :large,
    :unavailable,
    :member_count,
    :voice_states,
    :members,
    :channels,
    :presences
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
  @type owner_id :: Snowflake.t()

  @typedoc "The id of the voice region"
  @type region :: String.t()

  @typedoc "The id of the guild's afk channel"
  @type afk_channel_id :: Snowflake.t() | nil

  @typedoc "The time someone must be afk before being moved"
  @type afk_timeout :: integer

  @typedoc "Whether the guild is emeddable"
  @type embed_enabled :: boolean | nil

  @typedoc "The id of the embedded channel"
  @type embed_channel_id :: Snowflake.t() | nil

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
  @type roles :: [Role.t()]

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
  @type widget_channel_id :: Snowflake.t()

  @typedoc """
  The id of the channel to which system messages are sent.
  """
  @type system_channel_id :: Snowflake.t() | nil

  @typedoc "Date the guild was created"
  @type joined_at :: String.t() | nil

  @typedoc "Whether the guild is considered 'large'"
  @type large :: boolean | nil

  @typedoc "Whether the guild is avaliable"
  @type unavailable :: boolean | nil

  @typedoc "Total number of members in the guild"
  @type member_count :: integer | nil

  @typedoc "List of voice states as maps"
  @type voice_states :: list(map) | nil

  @typedoc "List of members"
  @type members :: [Member.t()] | nil

  @typedoc "List of channels"
  @type channels :: [Channel.t()] | nil

  @typedoc "List of simple presence maps"
  @type presences :: [map]

  @type t :: %__MODULE__{
          id: id,
          name: name,
          icon: icon,
          splash: splash,
          owner_id: owner_id,
          region: region,
          afk_channel_id: afk_channel_id,
          afk_timeout: afk_timeout,
          embed_enabled: embed_enabled,
          embed_channel_id: embed_channel_id,
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
          joined_at: joined_at,
          large: large,
          unavailable: unavailable,
          member_count: member_count,
          voice_states: voice_states,
          members: members,
          channels: channels,
          presences: presences
        }

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
      |> Map.update(:roles, nil, &Util.cast(&1, {:list, {:struct, Role}}))
      |> Map.update(:emojis, nil, &Util.cast(&1, {:list, {:struct, Emoji}}))
      |> Map.update(:application_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:widget_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:system_channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:members, nil, &Util.cast(&1, {:list, {:struct, Member}}))
      |> Map.update(:channels, nil, &Util.cast(&1, {:list, {:struct, Channel}}))

    struct(__MODULE__, new)
  end
end
