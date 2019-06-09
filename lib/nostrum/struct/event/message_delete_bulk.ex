defmodule Nostrum.Struct.Event.MessageDeleteBulk do
  defstruct [
    :channel_id,
    :guild_id,
    :ids
  ]

  @type t :: %__MODULE__{
    channel_id: Snowflake.t(),
    guild_id: Snowflake.t() | nil,
    ids: [Snowflake.t(), ...]
  }
end
