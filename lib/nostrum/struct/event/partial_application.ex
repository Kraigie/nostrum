defmodule Nostrum.Struct.Event.PartialApplication do
  @moduledoc "Sent on `READY`"
  @moduledoc since: "0.5.0"

  alias Nostrum.{Snowflake, Util}

  defstruct [:id, :flags]

  @typedoc "ID of the application"
  @type id :: Snowflake.t()

  @typedoc """
  Public flags of the application.

  See https://discord.com/developers/docs/resources/application#application-object-application-flags
  """
  @type flags :: non_neg_integer()

  @typedoc "Event sent as part of the `READY` payload."
  @type t :: %__MODULE__{
          id: id,
          flags: flags
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
