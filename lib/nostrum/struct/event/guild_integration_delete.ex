defmodule Nostrum.Struct.Event.GuildIntegrationDelete do
  @moduledoc """
  Event fired when a guild integration is deleted.
  """
  @moduledoc since: "0.5.1"

  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Integration
  alias Nostrum.Struct.Guild.Integration.Application, as: IntegrationApplication
  alias Nostrum.{Snowflake, Util}

  defstruct [:id, :guild_id, :application_id]

  @typedoc """
  The id of the deleted integration.
  """
  @type id :: Integration.id()

  @typedoc """
  The id of the guild the integration is in.
  """
  @type guild_id :: Guild.id()

  @typedoc """
  id of the bot/OAuth2 application for this discord integration
  """
  @type application_id :: IntegrationApplication.id() | nil

  @type t :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          application_id: application_id
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:application_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
