defmodule Nostrum.Struct.VoiceState do
  @moduledoc """
  Struct representing a Discord guild.
  """

  alias Nostrum.Util

  @typedoc "The guild's id"
  @type guild_id :: integer | nil

  @typedoc "The channel id this user is connected to."
  @type channel_id :: integer | nil

  @typedoc "The user's id"
  @type user_id :: integer

  @typedoc "The session's id"
  @type session_id :: integer

  @typedoc "Whether this user is deafened by the server"
  @type deaf :: boolean

  @typedoc "Whether this user is muted by the server"
  @type mute :: boolean

  @typedoc "Whether this user is locally deafened"
  @type self_deaf :: boolean

  @typedoc "Whether this user is locally muted"
  @type self_mute :: boolean

  @typedoc "Not mentioned in the document."
  @type self_video :: boolean

  @typedoc "Whether this user is muted by the current user"
  @type suppress :: boolean

  @type t :: %__MODULE__{
    guild_id: guild_id,
    channel_id: channel_id,
    session_id: session_id,
    user_id: user_id,
    deaf: deaf,
    mute: mute,
    self_deaf: self_deaf,
    self_mute: self_mute,
    self_video: self_video,
    suppress: suppress
  }

  @derive [Poison.Encoder]
  defstruct [
    :guild_id,
    :channel_id,
    :session_id,
    :user_id,
    :deaf,
    :mute,
    :self_deaf,
    :self_mute,
    :self_video,
    :suppress
  ]

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  def to_struct(map) do
    struct(__MODULE__, map)
  end
end