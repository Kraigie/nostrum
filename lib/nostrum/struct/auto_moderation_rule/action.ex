defmodule Nostrum.Struct.AutoModerationRule.Action do
  @moduledoc """
  Defines an action to be taken when a rule is triggered.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Struct.AutoModerationRule.ActionMetadata
  alias Nostrum.Util

  defstruct [
    :type,
    :metadata
  ]

  @typedoc """
  The type of action to be taken.

  | value | action | description
  | ---- | ---- | -----------
  |`1` | `BLOCK_MESSAGE` | Blocks the message from being created
  | `2` | `SEND_ALERT_MESSAGE` | Logs the content of the message in the specified channel
  | `3` | `TIMEOUT` | timeout a user for a specified duration
  | `4` | `BLOCK_MEMBER_INTERACTION` | prevents a member from using text, voice, or other interactions
  """
  @type action_type :: 1..4

  @type metadata :: ActionMetadata.t()

  @type t :: %__MODULE__{
          type: action_type(),
          metadata: metadata()
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:metadata, nil, &Util.cast(&1, {:struct, ActionMetadata}))

    struct(__MODULE__, new)
  end
end
