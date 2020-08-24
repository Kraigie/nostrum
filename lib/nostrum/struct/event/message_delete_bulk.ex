defmodule Nostrum.Struct.Event.MessageDeleteBulk do
  @moduledoc """
  Struct representing a Message Delete Bulk event
  """

  alias Nostrum.Snowflake

  defstruct [
    :channel_id,
    :guild_id,
    :ids
  ]

  @typedoc "Channel id of the deleted message"
  @type channel_id :: Snowflake.t()

  @typedoc """
  Guild id of the deleted message

  `nil` if a non-guild message was deleted.
  """
  @type guild_id :: Snowflake.t() | nil

  @typedoc "Ids of the deleted messages"
  @type ids :: [Snowflake.t(), ...]

  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id,
          ids: ids
        }

  @doc false
  def to_struct(map), do: struct(__MODULE__, map)
end
