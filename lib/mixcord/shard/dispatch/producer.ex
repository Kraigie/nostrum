defmodule Mixcord.Shard.Dispatch.Producer do
  @moduledoc """
  Gen Stage dispatch producer.

  # Consuming Gateway Events
  To handle events, Mixcord uses a GenStage implementation. GenStage is "new" with
  Elixir version 1.4, expanding on the old functionality of GenEvent.

  Mixcord defines the `producer` in the GenStage design. To consume the events you must
  create at least one `consumer` process that is linked to Mixcord's producer.

  It is generally recommended that you spawn a consumer per core. To find this
  number you can use `System.schedulers_online/0`.
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
