defmodule Nostrum.Struct.ApplicationCommandInteractionData do
  @moduledoc "Struct for interaction data."

  alias Nostrum.Snowflake

  alias Nostrum.Struct.{
    ApplicationCommandInteractionDataOption,
    ApplicationCommandInteractionDataResolved
  }

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
    :target_id
  ]

  @typedoc "ID of the invoked command"
  @type id :: Snowflake.t() | nil

  @typedoc "Name of the invoked command"
  @type name :: String.t() | nil

  @typedoc "Converted users & roles & channels"
  @typedoc since: "0.5.0"
  @type resolved :: ApplicationCommandInteractionDataResolved.t() | nil

  @typedoc "Parameters and values supplied by the user, if applicable"
  @type options :: ApplicationCommandInteractionDataOption.t() | nil

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

  @typedoc "id of the user or message targeted by a context menu command"
  @typedoc since: "0.5.0"
  @type target_id :: Snowflake.t() | nil

  @typedoc """
  For select menu components, this will be a list of the values the user selected.
  """
  @typedoc since: "0.5.0"
  @type select_values :: [String.t()] | nil

  @typedoc """
  Command interaction data for slash commands.

  Used as part of `t:Nostrum.Struct.Interaction.t/0`.

  Official reference:
  https://discord.com/developers/docs/interactions/slash-commands#interaction-applicationcommandinteractiondata
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
          target_id: target_id
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    %__MODULE__{
      id: map[:id],
      name: map[:name],
      type: map[:type],
      resolved: Util.cast(map[:resolved], {:struct, ApplicationCommandInteractionDataResolved}),
      options:
        Util.cast(map[:options], {:list, {:struct, ApplicationCommandInteractionDataOption}}),
      custom_id: map[:custom_id],
      component_type: map[:component_type],
      values: map[:values],
      target_id: map[:target_id]
    }
  end
end
