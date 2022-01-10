defmodule Nostrum.Struct.Event.VoiceState do
  @moduledoc "Represents a user's voice connection status"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User
  alias Nostrum.Util

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
    %__MODULE__{
      guild_id: map.guild_id,
      channel_id: map.channel_id,
      user_id: map.user_id,
      member: Util.cast(map[:member], {:struct, Member}),
      session_id: map.session_id,
      deaf: map.deaf,
      mute: map.mute,
      self_deaf: map.self_deaf,
      self_mute: map.self_mute,
      self_stream: map[:self_stream] || false,
      self_video: map.self_video,
      suppress: map.suppress,
      request_to_speak_timestamp: Util.maybe_to_datetime(map.request_to_speak_timestamp)
    }
  end
end
