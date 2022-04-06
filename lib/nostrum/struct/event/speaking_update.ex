defmodule Nostrum.Struct.Event.SpeakingUpdate do
  @moduledoc """
  Struct representing a Nostrum-generated Speaking Update event

  Nostrum will generate this event when the bot starts or stops playing audio.
  """

  defstruct [
    :channel_id,
    :guild_id,
    :speaking,
    :current_url,
    :timed_out
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

  @typedoc """
  Current URL being played if a readable format.
  """
  @typedoc since: "0.6.0"
  @type current_url :: String.t() | nil

  @typedoc """
  Boolean representing if speaking update was caused by an audio timeout.
  """
  @typedoc since: "0.5.0"
  @type timed_out :: boolean()

  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id,
          speaking: speaking,
          current_url: current_url,
          timed_out: timed_out
        }

  @doc false
  def to_struct(map), do: struct(__MODULE__, map)
end
