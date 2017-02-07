defmodule Mixcord.Shard.Dispatch.Producer do
  @moduledoc """
  Gen Stage dispatch producer
  """

  use GenStage

  def start_link do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:producer, {:queue.new, 0}, dispatcher: GenStage.DemandDispatcher}
  end

  def notify(payload) do
    GenState.cast(__MODULE__, {:notify, payload})
  end

  def handle_cast({:notify, payload}, {queue, demand}) do
    # HANDLE REQUESTS INTERNALLY HERE
    dispatch_events(:queue.in(payload, queue), demand, [])
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