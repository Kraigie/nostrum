defmodule Nostrum.Shard.Producer do
  @moduledoc false

  use GenStage

  require Logger

  def start_link(id) do
    GenStage.start_link(__MODULE__, id)
  end

  def init(id) do
    # Store list of pids for consumers to connect to
    Registry.register(ProducerRegistry, :pids, id)
    # Store shard_num to dispatch events to shards' producer
    Registry.register(ProducerRegistry, id, id)
    {:producer, {:queue.new, 0}, dispatcher: GenStage.DemandDispatcher}
  end

  # Handle any errors from cache methods
  def notify(_pid, {event, {:error, reason}}, _state),
    do: Logger.warn("ERROR PROCESSING #{inspect event}: #{inspect reason}")
  def notify(pid, payload, state) do
    GenStage.cast(pid, {:notify, payload, state})
  end

  def payload_to_tuple(payload) when is_tuple(payload), do: payload
  def payload_to_tuple(payload), do: {payload}

  def build_event(event_name, event_payload, ws_state) do
    {event_name, payload_to_tuple(event_payload), ws_state}
  end

  def handle_cast({:notify, {event, payload}, state}, {queue, demand}) do
    dispatch_events(:queue.in(build_event(event, payload, state), queue), demand, [])
  end

  def handle_demand(incoming_demand, {queue, demand}) do
    dispatch_events(queue, demand + incoming_demand, [])
  end

  def dispatch_events(queue, demand, events) do
    with d when
      d > 0 <- demand,
      {{:value, payload}, queue} <- :queue.out(queue)
    do
      dispatch_events(queue, demand - 1, [payload | events])
    else
      _ -> {:noreply, Enum.reverse(events), {queue, demand}}
    end
  end
end
