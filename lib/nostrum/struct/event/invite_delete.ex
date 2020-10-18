defmodule Nostrum.Struct.Event.InviteDelete do
  @moduledoc """
  Struct representing an Invite Delete event
  """

  alias Nostrum.Struct.{Channel, Guild}

  defstruct [
    :channel_id,
    :guild_id,
    :code
  ]

  @typedoc """
  Channel id of the channel this invite is for.
  """
  @type channel_id :: Channel.id()

  @typedoc """
  Guild id of the guild this invite is for.
  """
  @type guild_id :: Guild.id() | nil

  @typedoc """
  The unique invite code.
  """
  @type code :: String.t()

  @type t :: %__MODULE__{
          channel_id: channel_id,
          guild_id: guild_id,
          code: code
        }

  @doc false
  def to_struct(map), do: struct(__MODULE__, map)
end
