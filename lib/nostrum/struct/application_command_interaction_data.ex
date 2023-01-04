defmodule Nostrum.Struct.ApplicationCommandInteractionData do
  @moduledoc "Struct for interaction data."

  alias Nostrum.Struct.{
    ApplicationCommandInteractionDataOption,
    ApplicationCommandInteractionDataResolved,
    Message.Component
  }

  alias Nostrum.Snowflake
  alias Nostrum.Util

  defstruct [
    :id,
    :name,
    :resolved,
    :options,
    :custom_id,
    :component_type,
    :type,
    :values,
    :target_id,
    :components
  ]

  @typedoc "ID of the invoked command"
  @type id :: Snowflake.t() | nil

  @typedoc "Name of the invoked command"
  @type name :: String.t() | nil

  @typedoc "Converted users & roles & channels"
  @typedoc since: "0.5.0"
  @type resolved :: ApplicationCommandInteractionDataResolved.t() | nil

  @typedoc "Parameters and values supplied by the user, if applicable"
  @type options :: [ApplicationCommandInteractionDataOption.t()] | nil

  @typedoc "For components, the ``custom_id`` of the component"
  @typedoc since: "0.5.0"
  @type custom_id :: String.t() | nil

  @typedoc "For components, the ``type`` of the component"
  @typedoc since: "0.5.0"
  @type component_type :: integer() | nil

  @typedoc """
  The type of application command invoked.
  Official reference:
  https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-types
  """
  @typedoc since: "0.5.0"
  @type interaction_type :: integer() | nil

  @typedoc "ID of the user or message targeted by a context menu command"
  @typedoc since: "0.5.0"
  @type target_id :: Snowflake.t() | nil

  @typedoc """
  For select menu components, this will be a list of the values the user selected.
  """
  @typedoc since: "0.5.0"
  @type select_values :: [String.t()] | nil

  @typedoc """
  For Modal Sumbit interactions, this will contain the values the user submitted.
  """
  @typedoc since: "0.5.1"
  @type components :: [Component.t()] | nil

  @typedoc """
  Command interaction data for slash commands.

  Used as part of `t:Nostrum.Struct.Interaction.t/0`.

  Official reference:
  https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object
  """
  @type t :: %__MODULE__{
          id: id,
          name: name,
          type: interaction_type,
          resolved: resolved,
          options: options,
          custom_id: custom_id,
          component_type: component_type,
          values: select_values,
          target_id: target_id,
          components: components
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:target_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(
        :resolved,
        nil,
        &Util.cast(&1, {:struct, ApplicationCommandInteractionDataResolved})
      )
      |> Map.update(
        :options,
        nil,
        &Util.cast(&1, {:list, {:struct, ApplicationCommandInteractionDataOption}})
      )
      |> Map.update(:components, nil, &Util.cast(&1, {:list, {:struct, Component}}))

    struct(__MODULE__, new)
  end
end
