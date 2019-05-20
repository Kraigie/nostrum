defmodule Nostrum.Gateway.MessageDeleteEvent do
  @moduledoc """
  Struct representing a Message Delete Event.
  """

  alias Nostrum.Snowflake

  defstruct [
    :id,
    :channel_id,
    :guild_id
  ]

  @type id :: Snowflake.t()
  @type channel_id :: Snowflake.t()
  @type guild_id :: Snowflake.t() | nil

  @type t :: %__MODULE__{
          id: id,
          channel_id: channel_id,
          guild_id: guild_id
        }
end
