defmodule Nostrum.Struct.Message.Reference do
  @moduledoc """
  Struct representing a discord message reference.
  """

  alias Nostrum.{Snowflake, Util}
  alias Nostrum.Struct.{Channel, Guild, Message}

  defstruct [
    :message_id,
    :channel_id,
    :guild_id
  ]

  @typedoc "Id of the originating message"
  @type message_id :: Message.id()

  @typedoc "Id of the originating message's channel"
  @type channel_id :: Channel.id()

  @typedoc "Id of the originating message's guild"
  @type guild_id :: Guild.id()

  @type t :: %__MODULE__{
          message_id: message_id,
          channel_id: channel_id,
          guild_id: guild_id
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
