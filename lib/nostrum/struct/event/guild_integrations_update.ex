defmodule Nostrum.Struct.Event.GuildIntegrationsUpdate do
  @moduledoc "Sent when a guild integration is updated"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Guild
  alias Nostrum.{Snowflake, Util}

  defstruct [:guild_id]

  @typedoc "ID of the guild whose integrations were updated"
  @type guild_id :: Guild.id()

  @typedoc "Event sent when a guild integration is updated"
  @type t :: %__MODULE__{
          guild_id: guild_id
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
