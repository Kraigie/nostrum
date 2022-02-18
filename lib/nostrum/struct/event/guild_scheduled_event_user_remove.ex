defmodule Nostrum.Struct.Event.GuildScheduledEventUserRemove do
  @moduledoc """
  Struct representing a guild scheduled event user remove event.
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.Struct.Guild.ScheduledEvent

  defstruct [
    :guild_scheduled_event_id,
    :user_id,
    :guild_id
  ]

  @typedoc """
  The id of the guild the event is scheduled for.
  """
  @type guild_id :: Guild.id()

  @typedoc """
  The id of the user that unsubscribed to the scheduled event.
  """
  @type user_id :: User.id()

  @typedoc """
  The id of the scheduled event.
  """
  @type guild_scheduled_event_id :: ScheduledEvent.id()

  @type t :: %__MODULE__{
          guild_scheduled_event_id: guild_scheduled_event_id,
          user_id: user_id,
          guild_id: guild_id
        }

  @doc false
  def to_struct(map), do: struct(__MODULE__, map)
end
