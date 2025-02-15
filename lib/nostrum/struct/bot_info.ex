defmodule Nostrum.Struct.BotInfo do
  @moduledoc """
  Represents metadata about a bot, passed as part of consumer events.
  """
  @moduledoc since: "0.11.0"

  @typedoc """
  Process that is handling this shard.
  """
  @type shard_session :: pid()

  @typedoc """
  Metadata about a nostrum bot.
  """
  @type t :: %__MODULE__{shard_session: shard_session()}

  defstruct [:shard_session]
end
