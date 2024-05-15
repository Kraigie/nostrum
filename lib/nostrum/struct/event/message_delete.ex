defmodule Nostrum.Struct.Event.MessageDelete do
  @moduledoc """
  Struct representing a Message Delete event
  """

  alias Nostrum.Struct.{Channel, Guild, Message}
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :channel_id,
    :guild_id,
    :deleted_message
  ]

  @typedoc "Id of the deleted message"
  @type id :: Message.id()

  @typedoc "Channel id of the deleted message"
  @type channel_id :: Channel.id()

  @typedoc """
  Guild id of the deleted message

  `nil` if a non-guild message was deleted.
  """
  @type guild_id :: Guild.id() | nil

  @typedoc """
  The deleted message, if it was found
  in the message cache.
  """
  @type deleted_message :: Message.t() | nil

  @type t :: %__MODULE__{
          id: id,
          channel_id: channel_id,
          guild_id: guild_id,
          deleted_message: deleted_message
        }

  @doc false
  def to_struct(map, deleted_message \\ nil) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.put(:deleted_message, deleted_message)

    struct(__MODULE__, new)
  end
end
