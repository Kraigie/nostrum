defmodule Mixcord.Struct.VoiceChannel do
  @moduledoc """
  Struct representing a Discord voice channel.
  """

  alias Mixcord.Struct.Overwrite
  alias Mixcord.Util

  @typedoc "The channe's id"
  @type id :: integer

  @typedoc "The id of the channel's guild"
  @type guild_id :: integer

  @typedoc "The name of the channel"
  @type name :: String.t

  @typedoc "The type of channel: 'Text' or 'Voice'"
  @type type :: String.t

  @typedoc "The ordered position of the channel"
  @type position :: integer

  @typedoc "Whether the channel is private"
  @type is_private :: boolean

  @typedoc "The list of overwrites"
  @type permission_overwrites :: list(Overwrite.t)

  @typedoc "The bitrate of the voice channel"
  @type bitrate :: integer

  @typedoc "The user limit of the voice channel"
  @type user_limit :: integer

  @type t :: %__MODULE__{
    id: id,
    guild_id: guild_id,
    name: name,
    type: type,
    position: position,
    is_private: is_private,
    permission_overwrites: permission_overwrites,
    bitrate: bitrate,
    user_limit: user_limit
  }

  @derive [Poison.Encoder]
  defstruct [
    :id,
    :guild_id,
    :name,
    :type,
    :position,
    :is_private,
    :permission_overwrites,
    :bitrate,
    :user_limit
  ]

  @doc false
  def to_struct(map) do
    new = map
    |> Map.update(:permission_overwrites, %{}, &Util.list_to_struct_list(&1, Overwrite))
    struct(__MODULE__, new)
  end
end
