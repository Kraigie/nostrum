defmodule Nostrum.Struct.Guild.Channel do
  @moduledoc """
  Struct representing a Discord guild channel.
  """

  alias Nostrum.Struct.Overwrite
  alias Nostrum.Util

  @typedoc "The channe's id"
  @type id :: integer

  @typedoc "The id of the channel's guild"
  @type guild_id :: integer

  @typedoc "The name of the channel"
  @type name :: String.t

  @typedoc "The type of the channel"
  @type type :: Intger

  @typedoc "The ordered position of the channel"
  @type position :: integer

  @typedoc "The list of overwrites"
  @type permission_overwrites :: list(Overwrite.t)

  @typedoc "Current channel topic, `nil` if voice channel"
  @type topic :: String.t | nil

  @typedoc "Id of the last message sent, `nil` if voice channel"
  @type last_message_id :: integer | nil

  @typedoc "The bitrate of the voice channel, `nil` if text channel"
  @type bitrate :: integer | nil

  @typedoc "The user limit of the voice channel, `nil` if text channel"
  @type user_limit :: integer | nil

  @type t :: %__MODULE__{
    id: id,
    guild_id: guild_id,
    name: name,
    type: type,
    position: position,
    permission_overwrites: permission_overwrites,
    topic: topic,
    last_message_id: last_message_id,
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
    :permission_overwrites,
    :topic,
    :last_message_id,
    :bitrate,
    :user_limit
  ]

  def p_encode do
    %__MODULE__{
      permission_overwrites: [Overwrite.p_encode]
    }
  end

  @doc false
  def to_struct(map) do
    new = map
    |> Map.update(:permission_overwrites, [], &Util.list_to_struct_list(&1, Overwrite))
    struct(__MODULE__, new)
  end

end
