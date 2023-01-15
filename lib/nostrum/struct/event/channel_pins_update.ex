defmodule Nostrum.Struct.Event.ChannelPinsUpdate do
  @moduledoc "Represents an update to channel pins."
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Util

  defstruct [:guild_id, :channel_id, :last_pin_timestamp]

  @typedoc "The ID of the guild, if the pin update was on a guild"
  @type guild_id :: Guild.id() | nil

  @typedoc "The ID of the channel"
  @type channel_id :: Channel.id()

  @typedoc "The time at which the most recent pinned message was pinned"
  @type last_pin_timestamp :: DateTime.t() | nil

  @typedoc "Event sent when a message is pinned or unpinned in a text channel"
  @type t :: %__MODULE__{
          guild_id: guild_id,
          channel_id: channel_id,
          last_pin_timestamp: last_pin_timestamp
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:last_pin_timestamp, nil, &Util.maybe_to_datetime/1)

    struct(__MODULE__, new)
  end
end
