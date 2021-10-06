defmodule Nostrum.Struct.Event.VoiceReady do
  @moduledoc since: "0.5.0"
  @moduledoc """
  Struct representing a Nostrum-generated Voice Ready event

  Nostrum will generate this event when the bot joins a voice channel
  and is ready to play audio.

  Listening to this event may be used for bots that begin playing audio
  directly after joining a voice channel as an alternative to waiting
  until `Nostrum.Voice.ready?/1` returns `true`.
  """

  defstruct [
    :channel_id,
    :guild_id
  ]

  alias Nostrum.Struct.{Channel, Guild}

  @typedoc """
  Id of the channel that voice is ready in.
  """
  @type channel_id :: Channel.id()

  @typedoc """
  Id of the guild that voice is ready in.
  """
  @type guild_id :: Guild.id()

  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id
        }

  @doc false
  def to_struct(map), do: struct(__MODULE__, map)
end
