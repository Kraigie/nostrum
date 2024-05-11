defmodule Nostrum.Shard.Event do
  @moduledoc false

  alias Nostrum.ConsumerGroup
  alias Nostrum.Shard.Dispatch
  alias Nostrum.Shard.Payload
  alias Nostrum.Struct.WSState
  alias Nostrum.Util

  require Logger

  @spec handle(atom(), map(), WSState.t()) ::
          {WSState.t() | {WSState.t(), reply :: iodata() | :reconnect}, [:gen_statem.action()]}
  def handle(:dispatch, payload, state) do
    payload = Util.safe_atom_map(payload)

    if Application.get_env(:nostrum, :log_dispatch_events),
      do: payload.t |> inspect() |> Logger.debug()

    {payload, state}
    |> Dispatch.handle()
    |> ConsumerGroup.dispatch()

    if payload.t == :READY do
      Logger.info("READY")

      {%{state | session: payload.d.session_id, resume_gateway: payload.d.resume_gateway_url}, []}
    else
      {state, []}
    end
  end

  def handle(:heartbeat, _payload, state) do
    Logger.debug("HEARTBEAT PING")
    {{state, Payload.heartbeat_payload(state.seq)}, []}
  end

  def handle(:heartbeat_ack, _payload, state) do
    Logger.debug("HEARTBEAT_ACK")
    {%{state | last_heartbeat_ack: DateTime.utc_now(), heartbeat_ack: true}, []}
  end

  def handle(:hello, payload, old_state) do
    state = Map.put(old_state, :heartbeat_interval, payload.d.heartbeat_interval)
    # Jitter the heartbeat as documented. But only for Hello - the subsequent
    # timeouts must not jitter it anymore, but send out at this interval.
    heartbeat_next = :rand.uniform(state.heartbeat_interval)
    heartbeat_action = {:state_timeout, heartbeat_next, :send_heartbeat}

    if session_exists?(state) do
      Logger.info("Resuming on shard session #{state.session} with seq #{state.seq}")
      {{state, Payload.resume_payload(state)}, heartbeat_action}
    else
      Logger.debug("Identifying with new shard session")
      {{state, Payload.identity_payload(state)}, heartbeat_action}
    end
  end

  def handle(:invalid_session, %{d: _can_resume? = true}, state) do
    Logger.info(
      "Invalid but resumable session. Attempting to resume at #{state.session} with seq #{state.seq}"
    )

    {{state, Payload.resume_payload(state)}, []}
  end

  def handle(:invalid_session, %{d: _can_resume? = false}, state) do
    Logger.info(
      "Invalid and un-resumable session at #{state.session} with seq #{state.seq}. Events may be lost."
    )

    {{%{state | session: nil, seq: nil}, :reconnect}, []}
  end

  def handle(:reconnect, _payload, state) do
    Logger.info("Asked to reconnect with session #{state.session}")
    {{state, :reconnect}, []}
  end

  def handle(event, _payload, state) do
    Logger.warning("UNHANDLED GATEWAY EVENT #{event}")
    {state, []}
  end

  def session_exists?(state) do
    not is_nil(state.session)
  end
end
