defmodule Nostrum.Struct.Event.PartialApplication do
  @moduledoc "Sent on `READY`"
  @moduledoc since: "0.5.0"

  alias Nostrum.Snowflake

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
    %__MODULE__{
      id: map.id,
      # This is documented as optional in Discord's Gateway documentation,
      # but unlike our other struct fields, we assume it to be present in
      # ready.
      flags: map.flags
    }
  end
end
