defmodule Nostrum.Struct.Message.Application do
  @moduledoc """
  Struct representing a Discord message application.
  """

  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :cover_image,
    :description,
    :icon,
    :name
  ]

  @typedoc "Id of the application"
  @type id :: Snowflake.t()

  @typedoc "Id of the embed's image asset"
  @type cover_image :: String.t()

  @typedoc "Application's description"
  @type description :: String.t()

  @typedoc "Id of the application's icon"
  @type icon :: String.t()

  @typedoc "Name of the application"
  @type name :: String.t()

  @type t :: %__MODULE__{
          id: id,
          cover_image: cover_image,
          description: description,
          icon: icon,
          name: name
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
