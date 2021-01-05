defmodule Nostrum.Struct.ApplicationCommandInteractionDataOption do
  @moduledoc "Struct for command invocation arguments."

  # seasons greetings from `AbstractBeanModelParserFactory.java`.

  alias Nostrum.Util

  defstruct [:name, :value, :options]

  @typedoc "Parameter name"
  @type name :: String.t()

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
          value: value,
          options: options
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(
        :options,
        nil,
        &Util.cast(&1, {:list, {:struct, ApplicationCommandInteractionDataOption}})
      )

    struct(__MODULE__, new)
  end
end
