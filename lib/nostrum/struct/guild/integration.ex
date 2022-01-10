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

  defstruct [:id, :name, :type, :enabled]

  @typedoc "Snowflake ID of the integration"
  @type id :: Snowflake.t()

  @typedoc "Name of the integration"
  @type name :: String.t()

  @typedoc "Integration type (Twitch, YouTube or Discord)"
  @type type :: String.t()

  @typedoc "Whether this integration is enabled"
  @type enabled :: boolean()

  @typedoc "Represents a Guild integration"
  @type t :: %__MODULE__{
          id: id,
          name: name,
          type: type,
          enabled: enabled
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
