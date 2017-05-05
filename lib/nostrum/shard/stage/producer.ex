defmodule Nostrum.Shard.Stage.Producer do
  @moduledoc false

  use GenStage

  require Logger

  def start_link do
    GenStage.start_link(__MODULE__, [])
  end

  def init(_) do
    {:producer, {:queue.new, 0}, dispatcher: GenStage.DemandDispatcher}
  end

  def notify(pid, payload, state) do
    GenStage.cast(pid, {:notify, payload, state})
  end

  def handle_cast({:notify, payload, state}, {queue, demand}) do
    dispatch_events(:queue.in({payload, state}, queue), demand, [])
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
