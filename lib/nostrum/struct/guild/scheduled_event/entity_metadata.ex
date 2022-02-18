defmodule Nostrum.Struct.Guild.ScheduledEvent.EntityMetadata do
  @moduledoc """
  Struct representing any additional metadata associated with a Guild Event.
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Util

  @derive Jason.Encoder
  defstruct [
    :location
  ]

  @typedoc """
  The location of the event, 1-100 characters.

  Required for events with `entity_type` of `EXTERNAL`.
  """
  @type location :: String.t() | nil

  @type t :: %__MODULE__{
          location: location
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
