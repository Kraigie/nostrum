defmodule Nostrum.Struct.Event.SpeakingUpdate do
  @moduledoc """
  Struct representing a Nostrum-generated Speaking Update event

  Nostrum will generate this event when the bot starts or stops playing audio.
  """

  defstruct [
    :channel_id,
    :guild_id,
    :speaking
  ]

  alias Nostrum.Struct.{Channel, Guild}

  @typedoc """
  Id of the channel this speaking update is occurring in.
  """
  @type channel_id :: Channel.id()

  @typedoc """
  Id of the guild this speaking update is occurring in.
  """
  @type guild_id :: Guild.id()

  @typedoc """
  Boolean representing if bot has started or stopped speaking.
  """
  @type speaking :: boolean()

  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id,
          speaking: speaking
        }

  @doc false
  def to_struct(map), do: struct(__MODULE__, map)
end
