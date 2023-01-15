defmodule Nostrum.Struct.Event.ThreadListSync do
  @moduledoc """
  Struct representing a Thread List Sync event.

  This event is sent when the user gains access to a channel.
  """
  @moduledoc since: "0.5.1"

  alias Nostrum.Struct.{Channel, Guild, ThreadMember}
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :guild_id,
    :channel_ids,
    :threads,
    :members
  ]

  @typedoc """
  The id of the guild.
  """
  @type guid_id :: Guild.id()

  @typedoc """
  The parent channel ids whose threads are being synced.
  If omitted, all threads were synced for the entire guild.
  """
  @type channel_ids :: [Channel.id()] | nil

  @typedoc """
  All active threads in the given channels that the user can access.
  """
  @type threads :: [Channel.t()]

  @typedoc """
  All thread member objects from the synced threads for the current user,
  indicating which threads the user has been added to.
  """
  @type members :: [ThreadMember.t()]

  @type t :: %__MODULE__{
          guild_id: guid_id,
          channel_ids: channel_ids,
          threads: threads,
          members: members
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_ids, nil, &Util.cast(&1, {:list, Snowflake}))
      |> Map.update(:threads, nil, &Util.cast(&1, {:list, {:struct, Channel}}))
      |> Map.update(:members, nil, &Util.cast(&1, {:list, {:struct, ThreadMember}}))

    struct(__MODULE__, new)
  end
end
