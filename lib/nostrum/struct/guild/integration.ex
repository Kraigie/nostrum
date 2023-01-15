defmodule Nostrum.Struct.Guild.Integration do
  @moduledoc """
  Struct representing a Discord guild integration.

  The struct defined here only has the fields provided for Discord Bot
  integrations available. If you use Nostrum in a non-bot application, feel
  free to open an issue to add it.

  ## References

  - https://discord.com/developers/docs/resources/guild#integration-object
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.{Snowflake, Util}
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Integration.Account

  # Because Application is a module in the Standard Library, we should alias it with a different name
  alias Nostrum.Struct.Guild.Integration.Application, as: IntegrationApplication

  defstruct [:id, :name, :type, :enabled, :guild_id, :account, :application]

  @typedoc "Snowflake ID of the integration"
  @type id :: Snowflake.t()

  @typedoc """
  The id of the guild this integration is for.

  Only included when the Integration is sent via the gateway.
  """
  @typedoc since: "0.5.1"
  @type guild_id :: Guild.id() | nil

  @typedoc "Name of the integration"
  @type name :: String.t()

  @typedoc "Integration type (Twitch, YouTube or Discord)"
  @type type :: String.t()

  @typedoc "Whether this integration is enabled"
  @type enabled :: boolean()

  @typedoc """
  The integration account.
  """
  @typedoc since: "0.5.1"
  @type account :: Account.t()

  @typedoc """
  The bot/OAuth2 application for discord integrations
  """
  @typedoc since: "0.5.1"
  @type application :: IntegrationApplication.t() | nil

  @typedoc "Represents a Guild integration"
  @type t :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          name: name,
          type: type,
          enabled: enabled,
          account: account,
          application: application
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:account, nil, &Util.cast(&1, {:struct, Account}))
      |> Map.update(:application, nil, &Util.cast(&1, {:struct, IntegrationApplication}))

    struct(__MODULE__, new)
  end
end
