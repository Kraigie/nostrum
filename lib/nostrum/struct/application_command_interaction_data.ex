defmodule Nostrum.Struct.ApplicationCommandInteractionData do
  @moduledoc "Struct for interaction data."

  alias Nostrum.Snowflake
  alias Nostrum.Struct.ApplicationCommandInteractionDataOption
  alias Nostrum.Util

  defstruct [:id, :name, :options]

  @typedoc "ID of the invoked command"
  @type id :: Snowflake.t()

  @typedoc "Name of the invoked command"
  @type name :: String.t()

  @typedoc "Parameters and values supplied by the user, if applicable"
  @type options :: ApplicationCommandInteractionDataOption.t() | nil

  @typedoc """
  Command interaction data for slash commands.

  Used as part of `t:Nostrum.Struct.Interaction.t/0`.

  Official reference:
  https://discord.com/developers/docs/interactions/slash-commands#interaction-applicationcommandinteractiondata
  """
  @type t :: %__MODULE__{
          id: id,
          name: name,
          options: options
        }

  @doc false
  @spec to_struct(Map.t()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(
        :options,
        nil,
        &Util.cast(&1, {:list, {:struct, ApplicationCommandInteractionDataOption}})
      )

    struct(__MODULE__, new)
  end
end
