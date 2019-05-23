defmodule Nostrum.Gateway.MessageDeleteEvent do
  @moduledoc """
  Struct representing a Message Delete Event
  """

  alias Nostrum.Snowflake

  defstruct [
    :id,
    :channel_id,
    :guild_id
  ]

  @typedoc "Id of the deleted message"
  @type id :: Snowflake.t()

  @typedoc "Channel id of the deleted message"
  @type channel_id :: Snowflake.t()

  @typedoc """
  Guild id of the deleted message

  `nil` if a non-guild message was deleted.
  """
  @type guild_id :: Snowflake.t() | nil

  @type t :: %__MODULE__{
          id: id,
          channel_id: channel_id,
          guild_id: guild_id
        }
end
