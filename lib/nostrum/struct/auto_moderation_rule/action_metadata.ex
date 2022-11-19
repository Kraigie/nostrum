defmodule Nostrum.Struct.AutoModerationRule.ActionMetadata do
  @moduledoc """
  Struct representing any additional data used when an action is taken.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.{Snowflake, Util}

  defstruct [
    :channel_id,
    :duration_seconds
  ]

  @typedoc """
  The id of the channel to send an alert message to.
  """
  @type send_alert_message_metadata :: %__MODULE__{
          channel_id: Snowflake.t()
        }

  @typedoc """
  The number of seconds to timeout the user for,
  has a maximum of 2419200 seconds (4 weeks).
  """
  @type timeout_metadata :: %__MODULE__{
          duration_seconds: 1..2_419_200
        }

  @typedoc """
  The type of metadata present depends on the action type.

  | value | type
  | ---- | ----
  | `channel_id` | `SEND_ALERT_MESSAGE`
  | `duration_seconds` | `TIMEOUT`
  """
  @type t :: send_alert_message_metadata() | timeout_metadata()

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
