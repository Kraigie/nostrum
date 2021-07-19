defmodule Nostrum.Struct.ApplicationCommandInteractionDataOption do
  @moduledoc "Struct for command invocation arguments."

  # seasons greetings from `AbstractBeanModelParserFactory.java`.

  alias Nostrum.Util

  defstruct [:name, :type, :value, :options]

  @typedoc "Parameter name"
  @type name :: String.t()

  @typedoc """
  The application command option type.

  See https://discord.com/developers/docs/interactions/slash-commands#application-command-object-application-command-option-type
  for more details.
  """
  @typedoc since: "0.5.0"
  @type type :: 1..9

  @typedoc """
  Parameter value.

  Mutually exclusive with `options`.
  """
  # OptionType.t() ?
  @type value :: String.t() | nil

  @typedoc """
  Parameter options for subcommands.

  Mutually exclusive with `value`.
  """
  @type options :: [__MODULE__.t()] | nil

  @typedoc "Command interaction data struct"
  @type t :: %__MODULE__{
          name: name,
          type: type,
          value: value,
          options: options
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    %__MODULE__{
      name: map["name"],
      type: map["type"],
      value: map["value"],
      options:
        Util.cast(map["options"], {:list, {:struct, ApplicationCommandInteractionDataOption}})
    }
  end
end
