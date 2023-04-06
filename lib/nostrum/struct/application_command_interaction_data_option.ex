defmodule Nostrum.Struct.ApplicationCommandInteractionDataOption do
  @moduledoc "Struct for command invocation arguments."

  # seasons greetings from `AbstractBeanModelParserFactory.java`.

  alias Nostrum.Snowflake
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [:name, :type, :value, :options, :focused]

  @typedoc "Parameter name"
  @type name :: String.t()

  @typedoc """
  The application command option type.

  See https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-option-type
  for more details.

  You can use one of the `Nostrum.Constants.ApplicationCommandOptionType` methods.
  """
  @typedoc since: "0.5.0"
  @type type :: 1..10

  @typedoc """
  Parameter value.

  The type of this depends on the `t:type/0`:

  - For `t:type/0` of `3`, this will be a `t:String.t/0`.
  - For `t:type/0` of `4`, this will be a `t:integer/0`.
  - For `t:type/0` of `5`, this will be a `t:boolean/0`.
  - For `t:type/0` of `6`, this will be a `t:Nostrum.Struct.User.id/0`. The
    corresponding guild member _and_ user can be looked up in
    `t:Nostrum.Struct.ApplicationCommandInteractionData.resolved/0`.
  - For `t:type/0` of `7`, this will be a `t:Nostrum.Struct.Channel.id/0`. The
    corresponding channel can be looked up in
    `t:Nostrum.Struct.ApplicationCommandInteractionData.resolved/0`.
  - For `t:type/0` of `8`, this will be a `t:Nostrum.Struct.Guild.Role.id/0`. The
    corresponding role can be looked up in
    `t:Nostrum.Struct.ApplicationCommandInteractionData.resolved/0`.
  - For `t:type/0` of `9`, this will be a `t:Nostrum.Struct.User.id/0` or `t:Nostrum.Struct.Guild.Role.id/0`. The
    corresponding user or role can be looked up in
    `t:Nostrum.Struct.ApplicationCommandInteractionData.resolved/0`.
  - For `t:type/0` of `10`, this will be a `t:number/0`.

  Mutually exclusive with `options`. If `options` is not `nil`, this will be `nil`.
  """
  @type value ::
          String.t()
          | integer()
          | boolean()
          | number()
          | User.id()
          | Channel.id()
          | Role.id()
          | nil

  @typedoc """
  Parameter options for subcommands.

  Mutually exclusive with `value`.
  """
  @type options :: [__MODULE__.t()] | nil

  @typedoc """
  Whether this parameter is focused for `autocomplete` interactions.
  """
  @typedoc since: "0.5.0"
  @type focused :: boolean() | nil

  @typedoc "Command interaction data struct"
  @type t :: %__MODULE__{
          name: name,
          type: type,
          value: value,
          options: options,
          focused: focused
        }

  defp parse_value(type, value) when type in [6, 7, 8, 9], do: Util.cast(value, Snowflake)
  defp parse_value(_type, value), do: value

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:options, nil, &Util.cast(&1, {:list, {:struct, __MODULE__}}))
      |> Map.update(:value, nil, &parse_value(map.type, &1))

    struct(__MODULE__, new)
  end
end
