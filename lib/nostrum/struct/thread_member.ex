defmodule Nostrum.Struct.ThreadMember do
  @moduledoc """
  Struct representing a thread member object
  """

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :user_id,
    :join_timestamp,
    :flags,
    :guild_id
  ]

  @typedoc """
  The id of the thread, omitted within `GUILD_CREATE` events
  """
  @typedoc since: "0.5.1"
  @type id :: Snowflake.t() | nil

  @typedoc """
  The id of the user, omitted within `GUILD_CREATE` events
  """
  @typedoc since: "0.5.1"
  @type user_id :: User.id() | nil

  @typedoc """
  The timestamp of when the user last joined the thread
  """
  @typedoc since: "0.5.1"
  @type join_timestamp :: DateTime.t()

  @typedoc """
  Any user-thread settings
  """
  @type flags :: non_neg_integer()

  @typedoc """
  The id of the guild the thread is in.

  Only present within `THREAD_MEMBER_UPDATE` events
  """
  @typedoc since: "0.5.1"
  @type guild_id :: Guild.id() | nil

  @type t :: %__MODULE__{
          id: id,
          user_id: user_id,
          join_timestamp: join_timestamp,
          flags: flags,
          guild_id: guild_id
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:join_timestamp, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
