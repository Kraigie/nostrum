defmodule Nostrum.Struct.Event.VoiceServerUpdate do
  @moduledoc "Sent when a guild's voice server is updated"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Guild

  defstruct [:token, :guild_id, :endpoint]

  @typedoc "Voice connection token"
  @type token :: String.t()

  @typedoc "Guild this voice server update is for"
  @type guild_id :: Guild.id()

  @typedoc "The voice server host"
  @type endpoint :: String.t() | nil

  @typedoc "Event sent when a guild's voice server is updated"
  @type t :: %__MODULE__{
          token: token,
          guild_id: guild_id,
          endpoint: endpoint
        }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, map)
  end
end
