defmodule Nostrum.Struct.Guild.ScheduledEvent do
  @moduledoc """
  Struct representing a scheduled event in a guild.
  """

  alias Nostrum.Struct.Guild.ScheduledEvent.EntityMetadata
  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :guild_id,
    :channel_id,
    :creator_id,
    :name,
    :description,
    :scheduled_start_time,
    :scheduled_end_time,
    :privacy_level,
    :status,
    :entity_type,
    :entity_id,
    :entity_metadata,
    :creator,
    :user_count
  ]

  @typedoc "The id of the scheduled event."
  @type id :: Snowflake.t()

  @typedoc "The id of the guild the scheduled event is in."
  @type guild_id :: Snowflake.t()

  @typedoc """
  The id of the channel the scheduled event is in.
  Will be `nil` if `entity_type` is `EXTERNAL`
  """
  @type channel_id :: Snowflake.t() | nil

  @typedoc """
  The id of the user who created the scheduled event.

  note: This will be `nil` only for events created before October 25th, 2021.
  """
  @type creator_id :: Snowflake.t() | nil

  @typedoc "The name of the scheduled event."
  @type name :: String.t()

  @typedoc "The description of the scheduled event."
  @type description :: String.t() | nil

  @typedoc """
  The time the scheduled event starts.
  """
  @type scheduled_start_time :: DateTime.t()

  @typedoc """
  The time the scheduled event ends as an ISO8601 timestamp.
  Only required if `entity_type` is `EXTERNAL`
  """
  @type scheduled_end_time :: DateTime.t() | nil

  @typedoc """
  The privacy level of the scheduled event.
  At the time of writing, this is always `2` for `GUILD_ONLY`
  """
  @type privacy_level :: 2

  @typedoc """
  The status of the scheduled event.

  `1` - `SCHEDULED`
  `2` - `ACTIVE`
  `3` - `COMPLETED`
  `4` - `CANCELLED`

  note: Once status is set to Completed or Cancelled it can no longer be updated.
  """
  @type status :: 1..4

  @typedoc """
  The type of entity the scheduled event is for.

  `1` - `STAGE_INSTANCE`
  `2` - `VOICE`
  `3` - `EXTERNAL`
  """
  @type entity_type :: 1..3

  @typedoc """
  the id of an entity associated with a guild scheduled event.
  """
  @type entity_id :: Snowflake.t() | nil

  @typedoc """
  Holds additional metadata associated with a scheduled event.
  """
  @type entity_metadata :: EntityMetadata.t() | nil

  @typedoc """
  The user who created the scheduled event. Only present when retrieving the event from the API.

  note: This will be `nil` for events created before October 25th, 2021.
  """
  @type creator :: User.t() | nil

  @typedoc "The number of users who have subscribed to the scheduled event."
  @type user_count :: non_neg_integer() | nil

  @type t :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          channel_id: channel_id,
          creator_id: creator_id,
          name: name,
          description: description,
          scheduled_start_time: scheduled_start_time,
          scheduled_end_time: scheduled_end_time,
          privacy_level: privacy_level,
          status: status,
          entity_type: entity_type,
          entity_id: entity_id,
          entity_metadata: entity_metadata,
          creator: creator,
          user_count: user_count
        }

  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:creator_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:entity_metadata, nil, &Util.cast(&1, {:struct, EntityMetadata}))
      |> Map.update(:creator, nil, &Util.cast(&1, {:struct, User}))
      |> Map.update(:scheduled_start_time, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:scheduled_end_time, nil, &Util.maybe_to_datetime/1)

    struct(__MODULE__, new)
  end
end
