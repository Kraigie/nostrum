defmodule Nostrum.Struct.VoiceWSState do
  @moduledoc """
  Struct representing the current Voice WS state.
  """

  defstruct [
    :guild_id,
    :channel_id,
    :ssrc_map,
    :session,
    :token,
    :secret_key,
    :conn,
    :conn_pid,
    :stream,
    :gateway,
    :identified,
    :seq,
    :encryption_mode,
    :last_heartbeat_send,
    :last_heartbeat_ack,
    :heartbeat_ack,
    :heartbeat_interval,
    :heartbeat_ref
  ]

  @typedoc "The guild id that this voice websocket state applies to"
  @type guild_id :: Nostrum.Struct.Guild.id()

  @typedoc "The channel id that this voice websocket state applies to"
  @typedoc since: "0.6.0"
  @type channel_id :: Nostrum.Struct.Channel.id()

  @typedoc """
  A mapping of RTP SSRC (synchronization source) to user id

  This map can be used to identify the user who generated the incoming
  audio data when an RTP packet is received.
  """
  @typedoc since: "0.6.0"
  @type ssrc_map :: %{integer() => Nostrum.Struct.User.id()}

  @typedoc "The session id"
  @type session :: String.t()

  @typedoc "The session token"
  @type token :: String.t()

  @typedoc "The secret key for audio encryption"
  @typedoc since: "0.6.0"
  @type secret_key :: binary() | nil

  @typedoc "PID of the `:gun` worker connected to the websocket"
  @type conn :: pid()

  @typedoc "PID of the connection process"
  @type conn_pid :: pid()

  @typedoc "Stream reference for `:gun`"
  @type stream :: :gun.stream_ref()

  @typedoc "Gateway URL"
  @type gateway :: String.t()

  @typedoc "Whether the session has been identified"
  @type identified :: boolean()

  @typedoc "Sequence number for buffering server-sent events"
  @type seq :: integer()

  @typedoc "Encryption mode selected for voice channel"
  @type encryption_mode :: Nostrum.Voice.Crypto.cipher()

  @typedoc """
  The time the last heartbeat was sent, if a heartbeat hasn't been sent it
  will be the time the websocket process was started
  """
  @type last_heartbeat_send :: DateTime.t()

  @typedoc """
  The time the last heartbeat was acknowledged, will be nil if a heartbeat
  hasn't been ACK'd yet
  """
  @type last_heartbeat_ack :: DateTime.t() | nil

  @typedoc "Whether or not the last heartbeat sent was ACK'd"
  @type heartbeat_ack :: boolean()

  @typedoc "Interval at which heartbeats are sent"
  @type heartbeat_interval :: integer() | nil

  @typedoc "Time ref for the heartbeat"
  @type heartbeat_ref :: :timer.tref() | nil

  @type t :: %__MODULE__{
          guild_id: guild_id,
          channel_id: channel_id,
          ssrc_map: ssrc_map,
          session: session,
          token: token,
          secret_key: secret_key,
          conn: conn,
          conn_pid: conn_pid,
          stream: stream,
          gateway: gateway,
          identified: identified,
          seq: seq,
          encryption_mode: encryption_mode,
          last_heartbeat_send: last_heartbeat_send,
          last_heartbeat_ack: last_heartbeat_ack,
          heartbeat_ack: heartbeat_ack,
          heartbeat_interval: heartbeat_interval,
          heartbeat_ref: heartbeat_ref
        }
end
