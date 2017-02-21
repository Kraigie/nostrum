_ = """
      This module exists primarily for documentation/testing purposes
    """

defmodule Dummy do
  @moduledoc false

  def start_link do
    import Supervisor.Spec

    # List comprehension creates a consumer per cpu core
    children = for i <- 1..System.schedulers_online, do: worker(DummyConsumer, [], id: i)

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule DummyConsumer do
  @moduledoc false
  use Mixcord.Shard.Dispatch.Consumer
  require Logger

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  def handle_event({event_name, _, _}, _) do
    Logger.debug "User would handle #{event_name} here"
    {:ok, %{}}
  end
end

defmodule DummyConsumerOld do
  @moduledoc false

  use GenStage
  require Logger

  def start_link do
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
