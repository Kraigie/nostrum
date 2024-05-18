defmodule Nostrum.Struct.WSState do
  @moduledoc """
  Struct representing the current WS state.
  """

  defstruct [
    :shard_num,
    :total_shards,
    :seq,
    :session,
    :conn,
    :conn_pid,
    :stream,
    :gateway,
    :resume_gateway,
    :last_heartbeat_send,
    :last_heartbeat_ack,
    :heartbeat_ack,
    :heartbeat_interval,
    :compress_ctx
  ]

  @typedoc "The shard number"
  @type shard_num :: non_neg_integer

  @typedoc """
  The highest shard number for this bot.

  This may not be started locally, it is just used by nostrum to inform the
  gateway which events we are interested in.
  """
  @typedoc since: "0.8.0"
  @type total_shards :: non_neg_integer()

  @typedoc "The sequence number of the last event"
  @type seq :: integer | nil

  @typedoc "The session id"
  @type session :: integer | nil

  @typedoc "PID of the `:gun` worker connected to the websocket"
  @type conn :: pid

  @typedoc "PID of the connection process"
  @type conn_pid :: pid

  @typedoc "Stream reference for `:gun`"
  @typedoc since: "0.5.0"
  @type stream :: :gun.stream_ref()

  @typedoc "Gateway URL"
  @type gateway :: String.t()

  @typedoc "Gateway URL to use for resuming."
  @typedoc since: "0.9.0"
  @type resume_gateway :: String.t() | nil

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

  @typedoc "Whether or not the last hearbeat sent was ACK'd"
  @type heartbeat_ack :: boolean

  @typedoc "Interval at which heartbeats are sent"
  @type heartbeat_interval :: pos_integer() | nil

  @typedoc "Reference to the current compression context"
  @type compress_ctx :: reference | nil

  @type t :: %__MODULE__{
          shard_num: shard_num,
          total_shards: total_shards,
          seq: seq,
          session: session,
          conn: conn,
          conn_pid: conn_pid,
          stream: stream,
          gateway: gateway,
          resume_gateway: resume_gateway,
          last_heartbeat_send: last_heartbeat_send,
          last_heartbeat_ack: last_heartbeat_ack,
          heartbeat_ack: heartbeat_ack,
          heartbeat_interval: heartbeat_interval,
          compress_ctx: compress_ctx
        }
end
