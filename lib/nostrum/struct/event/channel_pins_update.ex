defmodule Nostrum.Struct.Event.ChannelPinsUpdate do
  @moduledoc "Represents an update to channel pins."
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild

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
    %__MODULE__{
      guild_id: map["guild_id"],
      channel_id: map["channel_id"],
      last_pin_timestamp: parse_stamp(map["last_pin_timestamp"])
    }
  end

  defp parse_stamp(nil) do
    nil
  end

  defp parse_stamp(stamp) do
    {:ok, casted, 0} = DateTime.from_iso8601(stamp)
    casted
  end
end
