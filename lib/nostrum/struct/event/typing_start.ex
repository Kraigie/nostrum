defmodule Nostrum.Struct.Event.TypingStart do
  @moduledoc "Sent when a user starts typing in a channel"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [:channel_id, :guild_id, :user_id, :timestamp, :member]

  @typedoc "Channel in which the user started typing"
  @type channel_id :: Channel.id()

  @typedoc "ID of the guild where the user started typing, if applicable"
  @type guild_id :: Guild.id() | nil

  @typedoc "ID of the user who started typing"
  @type user_id :: User.id()

  @typedoc "When the user started typing"
  @type timestamp :: DateTime.t()

  @typedoc "The member who started typing if this happened in a guild"
  @type member :: Member.t() | nil

  @typedoc "Event sent when a user starts typing in a channel"
  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id,
          user_id: user_id,
          timestamp: timestamp,
          member: member
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:member, nil, &Util.cast(&1, {:struct, Member}))
      |> Map.update(:timestamp, nil, &DateTime.from_unix!/1)

    struct(__MODULE__, new)
  end
end
