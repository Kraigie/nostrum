defmodule Nostrum.Struct.Guild.ScheduledEvent.User do
  @moduledoc """
  Struct representing a user in a guild's scheduled event.
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Guild.{Member, ScheduledEvent}
  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :guild_scheduled_event_id,
    :user,
    :member
  ]

  @typedoc "The ID of the guild scheduled event."
  @type event_id :: ScheduledEvent.id()

  @typedoc "The user which is subscribed to the event."
  @type user :: User.t()

  @typedoc "The guild member for the event"
  @type member :: Member.t() | nil

  @type t :: %__MODULE__{
          guild_scheduled_event_id: event_id,
          user: user,
          member: member
        }

  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:guild_scheduled_event_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))
      |> Map.update(:member, nil, &Util.cast(&1, {:struct, Member}))

    struct(__MODULE__, new)
  end
end
