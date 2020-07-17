defmodule Nostrum.Struct.Message.Activity do
  @moduledoc """
  Struct representing a Discord message activity.
  """

  alias Nostrum.Util

  defstruct [
    :type,
    :party_id
  ]

  @typedoc """
  [Type of message activity](https://discord.com/developers/docs/resources/channel#message-object-message-activity-types).
  """
  @type type :: integer

  @typedoc """
  The party id from a [rich presence event](https://discord.com/developers/docs/rich-presence/how-to).
  """
  @type party_id :: String.t() | nil

  @type t :: %__MODULE__{
          type: type,
          party_id: party_id
        }

  @doc false
  def to_struct(map) do
    new = map |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
