defmodule Mixcord.Shard.Dispatch.Producer do
  @moduledoc """
  Gen Stage dispatch producer
  """

  use GenStage
  alias Mixcord.Shard.Dispatch

  def start_link do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    {:producer, {:queue.new, 0}, dispatcher: GenStage.DemandDispatcher}
  end

  def notify(payload, state) do
    GenStage.cast(__MODULE__, {:notify, payload, state})
  end

  def handle_cast({:notify, payload, state}, {queue, demand}) do
    from_dispatch = Dispatch.handle(payload, state)
    dispatch_events(:queue.in({{payload.t, from_dispatch}, state}, queue), demand, [])
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

defmodule DummyConsumer do

  use GenStage
  require Logger

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    {:consumer, :ok, subscribe_to: [Mixcord.Shard.Dispatch.Producer]}
  end

  def handle_events(events, _from, state) do
    for event <- events do
      {{event_name, _payload}, _state} = event
      Logger.debug "User would process event #{event_name} here on pid #{inspect self()}"
    end
    {:noreply, [], state}
  end
end
