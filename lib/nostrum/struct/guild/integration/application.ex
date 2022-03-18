defmodule Nostrum.Struct.Guild.Integration.Application do
  @moduledoc """
  Struct representing a Discord Guild Integration Application.
  """
  @moduledoc since: "0.5.1"

  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [:id, :name, :icon, :description, :summary, :bot]

  @typedoc """
  The id of the application.
  """
  @type id :: Snowflake.t()

  @typedoc """
  The name of the application.
  """
  @type name :: String.t()

  @typedoc """
  The icon hash of the application.
  """
  @type icon :: String.t() | nil

  @typedoc """
  The description of the application.
  """
  @type description :: String.t()

  @typedoc """
  The summary of the application.
  """
  @type summary :: String.t()

  @typedoc """
  The bot associated with the application.
  """
  @type bot :: User.t() | nil

  @type t :: %__MODULE__{
          id: id,
          name: name,
          icon: icon,
          description: description,
          summary: summary,
          bot: bot
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:bot, nil, &Util.cast(&1, {:struct, User}))

    struct(__MODULE__, new)
  end
end
