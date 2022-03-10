defmodule Nostrum.Struct.Event.ThreadMembersUpdate do
  @moduledoc """
  Struct representing a thread members update event.

  This event is sent whenever a user is added or removed from a thread.

  If the current user does not have the `GUILD_MEMBERS` intent,
  this event will only be sent when the current user is added to or removed from a thread.
  """
  @moduledoc since: "0.5.1"

  defstruct [
    :id,
    :guild_id,
    :member_count,
    :added_members,
    :removed_member_ids
  ]

  alias Nostrum.Struct.{Channel, Guild, ThreadMember, User}
  alias Nostrum.{Snowflake, Util}

  @typedoc """
  The id of the thread.
  """
  @type id :: Channel.id()

  @typedoc """
  The id of the guild the thread is in.
  """
  @type guild_id :: Guild.id()

  @typedoc """
  The approximate number of members in the thread.

  This number is capped at 50, though there can be more members in the thread.
  """
  @type member_count :: non_neg_integer()

  @typedoc """
  The members that were added to the thread.
  """
  @type added_members :: [ThreadMember.t()] | nil

  @typedoc """
  The ids of the members that were removed from the thread.
  """
  @type removed_member_ids :: [User.id()] | nil

  @type t :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          member_count: member_count,
          added_members: added_members,
          removed_member_ids: removed_member_ids
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:added_members, nil, &Util.cast(&1, {:list, {:struct, ThreadMember}}))
      |> Map.update(:removed_member_ids, nil, &Util.cast(&1, {:list, Snowflake}))

    struct(__MODULE__, new)
  end
end
