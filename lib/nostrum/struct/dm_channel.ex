defmodule Nostrum.Struct.DMChannel do
  @moduledoc """
  Struct representing a Discord direct message channel.
  """

  alias Nostrum.Struct.User
  alias Nostrum.Util

  @typedoc "The channel's id"
  @type id :: integer

  @typedoc "Type of the channel"
  @type type :: integer

  @typedoc "The recipients of the message"
  @type recipients :: list(User.t)

  @typedoc "Id of the last message sent, should always be true for DMs"
  @type last_message_id :: integer

  @type t :: %__MODULE__{
    id: id,
    type: type,
    recipients: recipients,
    last_message_id: last_message_id
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :type,
    :recipients,
    :last_message_id
  ]

  @doc false
  def to_struct(map) do
    new = map
    |> Map.update(:recipients, [], &Util.list_to_struct_list(&1, User))
    struct(__MODULE__, new)
  end
end
