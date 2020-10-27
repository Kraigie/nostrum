defmodule Nostrum.Struct.WSState do
  @moduledoc """
  Struct representing the current WS state.
  """

  defstruct [
    :shard_num,
    :seq,
    :session,
    :shard_pid,
    :conn,
    :conn_pid,
    :gateway,
    :last_heartbeat_send,
    :last_heartbeat_ack,
    :heartbeat_ack,
    :heartbeat_interval,
    :heartbeat_ref,
    :zlib_ctx
  ]

  @typedoc "The shard number"
  @type shard_num :: non_neg_integer

  @typedoc "The sequence number of the last event"
  @type seq :: integer | nil

  @typedoc "The session id"
  @type session :: integer | nil

  @typedoc "PID of the shard containing this state"
  @type shard_pid :: pid

  @typedoc "PID of the `:gun` worker connected to the websocket"
  @type conn :: pid

  @typedoc "PID of the connection process"
  @type conn_pid :: pid

  @typedoc "Gateway URL"
  @type gateway :: String.t()

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
  @type heartbeat_interval :: integer | nil

  @typedoc "Time ref for the heartbeat"
  @type heartbeat_ref :: integer | nil

  @typedoc "Reference to the current zlib context"
  @type zlib_ctx :: reference | nil

  @type t :: %__MODULE__{
          shard_num: shard_num,
          seq: seq,
          session: session,
          shard_pid: shard_pid,
          conn: conn,
          conn_pid: conn_pid,
          gateway: gateway,
          last_heartbeat_send: last_heartbeat_send,
          last_heartbeat_ack: last_heartbeat_ack,
          heartbeat_ack: heartbeat_ack,
          heartbeat_interval: heartbeat_interval,
          heartbeat_ref: heartbeat_ref,
          zlib_ctx: zlib_ctx
        }
end
