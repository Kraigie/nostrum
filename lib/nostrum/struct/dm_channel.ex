defmodule Nostrum.Struct.DMChannel do
  @moduledoc """
  Struct representing a Discord direct message channel.
  """

  alias Nostrum.Struct.User

  @typedoc "The channel's id"
  @type id :: integer

  @typedoc "Type of the channel"
  @type type :: integer

  @typedoc "The recipient of the message"
  @type recipient :: User.t

  @typedoc "Id of the last message sent, should always be true for DMs"
  @type last_message_id :: integer

  @type t :: %__MODULE__{
    id: id,
    type: type,
    recipient: recipient,
    last_message_id: last_message_id
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :type,
    :recipient,
    :last_message_id
  ]

  @doc false
  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
