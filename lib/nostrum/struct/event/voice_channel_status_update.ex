defmodule Nostrum.Struct.Event.VoiceChannelStatusUpdate do
  @moduledoc "Sent when a voice channel's status is updated"
  @moduledoc since: "0.11.0"

  alias Nostrum.Struct.{Channel, Guild}

  defstruct [:id, :guild_id, :status]

  @typedoc "The voice channel ID"
  @type id :: Channel.id()

  @typedoc "Guild this event is for"
  @type guild_id :: Guild.id()

  @typedoc "The new channel status, `nil` when cleared"
  @type status :: String.t() | nil

  @typedoc "Event sent when a voice channel's status is updated"
  @type t :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          status: status
        }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
