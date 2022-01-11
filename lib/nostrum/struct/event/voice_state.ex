defmodule Nostrum.Struct.Event.VoiceState do
  @moduledoc "Represents a user's voice connection status"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :guild_id,
    :channel_id,
    :user_id,
    :member,
    :session_id,
    :deaf,
    :mute,
    :self_deaf,
    :self_mute,
    :self_stream,
    :self_video,
    :suppress,
    :request_to_speak_timestamp
  ]

  @typedoc "Guild ID this voice state is for, if applicable"
  @type guild_id :: Guild.id() | nil

  @typedoc "Channel ID this voice state is for"
  @type channel_id :: Channel.id()

  @typedoc "User this voice state is for"
  @type user_id :: User.id()

  @typedoc "Guild member this voice state is for, if applicable"
  @type member :: Member.t() | nil

  @typedoc "Session ID for this voice state"
  @type session_id :: String.t()

  @typedoc "Whether this user is deafened by the server"
  @type deaf :: boolean

  @typedoc "Whether this user is muteened by the server"
  @type mute :: boolean

  @typedoc "Whether this user is locally deafened"
  @type self_deaf :: boolean

  @typedoc "Whether this user is locally muted"
  @type self_mute :: boolean

  @typedoc "Whether the user is streaming using \"Go Live\""
  @type self_stream :: boolean

  @typedoc "Whether this user's camera is enabled"
  @type self_video :: boolean

  @typedoc "Whether this user is muted by the current user"
  @type suppress :: boolean

  @typedoc "Time at which the user requested to speak, if applicable"
  @type request_to_speak_timestamp :: DateTime.t() | nil

  @typedoc "Event sent when a user's voice status is updated"
  @type t :: %__MODULE__{
          guild_id: guild_id,
          channel_id: channel_id,
          user_id: user_id,
          member: member,
          session_id: session_id,
          deaf: deaf,
          mute: mute,
          self_deaf: self_deaf,
          self_mute: self_mute,
          self_stream: self_stream,
          self_video: self_video,
          suppress: suppress,
          request_to_speak_timestamp: request_to_speak_timestamp
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.put_new(:self_stream, false)
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:member, nil, &Util.cast(&1, {:struct, Member}))
      |> Map.update(:request_to_speak_timestamp, nil, &Util.maybe_to_datetime/1)

    struct(__MODULE__, new)
  end
end
