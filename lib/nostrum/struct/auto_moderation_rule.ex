defmodule Nostrum.Struct.AutoModerationRule do
  @moduledoc """
  Struct representing an auto-moderation rule.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Struct.AutoModerationRule.{Action, TriggerMetadata}
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :guild_id,
    :name,
    :creator_id,
    :event_type,
    :trigger_type,
    :trigger_metadata,
    :actions,
    :enabled,
    :exempt_roles,
    :exempt_channels
  ]

  @typedoc "The id of the auto-moderation rule"
  @type id :: Snowflake.t()

  @typedoc "The id of the guild the rule belongs to"
  @type guild_id :: Guild.id()

  @typedoc "The name of the rule"
  @type name :: String.t()

  @typedoc "The id of the user who created the rule"
  @type creator_id :: User.id()

  @typedoc """
  Indicates in what event context a rule should be checked

  | value | type | description
  | ---- | ---- | -----------
  |`1` | `MESSAGE_SEND` | when a member sends or edits a message in a guild
  """
  @type event_type :: 1

  @typedoc """
  Characters the type of content which triggered the rule

  | value | type | max per guild | description
  | ---- | ---- | ----- | -----------
  |`1` | `​KEYWORD` | 3 | check if content contains words from a user defined list of keywords
  | `2` | `HARMFUL_LINK` | 1 | check if the content contains any harmful links
  | `3` | `SPAM` | 1 | check if the content represents generic spam
  | `4` | `KEYWORD_PRESET `| 1 | check if the content contains a list of discord defined keywords

  note: `HARMFUL_LINK` and `SPAM` are not yet offically released at the time of this writing.
  """
  @type trigger_type :: 1..4

  @typedoc """
  Values which represent the different presets defined by Discord

  | value | type | description
  | ---- | ---- | -----------
  |`1` | `PROFANITY` | Words which may be considered profane
  | `2` | `HARMFUL_LINK` | Words that refer to sexually explicit behavior or activity
  | `3` | `SLURS` | Personal insults or words that may be considered hate speech
  """
  @type preset_values :: 1..3

  @typedoc """
  Additional data used to determine if the rule should triggered.

  The `t:trigger_type/0` field will determine which of the following fields are present.

  | key | associated `trigger_type`
  | ---- | -----------
  | `keywords` | `​KEYWORD`
  | `preset` | `KEYWORD_PRESET`
  """
  @type trigger_metadata :: TriggerMetadata.t()

  @typedoc """
  A list of Actions which will be performed when the rule is triggered.
  """
  @type actions :: [Action.t()]

  @typedoc """
  If the rule is enabled or not.
  """
  @type enabled :: boolean()

  @typedoc """
  A list of roles that are exempt from the rule.
  """
  @type exempt_roles :: [Role.id()]

  @typedoc """
  A list of channels that are exempt from the rule
  """
  @type exempt_channels :: [Channel.id()]

  @type t :: %__MODULE__{
          id: id(),
          guild_id: guild_id(),
          name: name(),
          creator_id: creator_id(),
          event_type: event_type(),
          trigger_type: trigger_type(),
          trigger_metadata: trigger_metadata(),
          actions: actions(),
          enabled: enabled(),
          exempt_roles: exempt_roles(),
          exempt_channels: exempt_channels()
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:creator_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:trigger_metadata, nil, &Util.cast(&1, {:struct, TriggerMetadata}))
      |> Map.update(:actions, nil, &Util.cast(&1, {:list, {:struct, Action}}))
      |> Map.update(:exempt_roles, nil, &Util.cast(&1, {:list, Snowflake}))
      |> Map.update(:exempt_channels, nil, &Util.cast(&1, {:list, Snowflake}))

    struct(__MODULE__, new)
  end
end
