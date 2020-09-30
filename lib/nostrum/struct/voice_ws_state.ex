defmodule Nostrum.Struct.VoiceWSState do
  @moduledoc false

  defstruct [
    :guild,
    :session,
    :token,
    :conn,
    :conn_pid,
    :gateway,
    :identified,
    :last_heartbeat_send,
    :last_heartbeat_ack,
    :heartbeat_ack,
    :heartbeat_interval,
    :heartbeat_ref,
  ]
end
