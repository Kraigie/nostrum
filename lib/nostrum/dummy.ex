_ = """
  This module exists primarily for documentation/testing purposes
"""

defmodule Dummy do
  @moduledoc false

  def start_link do
    import Supervisor.Spec

    # List comprehension creates a consumer per cpu core
    # children = for i <- 1..System.schedulers_online, do: worker(DummyConsumer, [], id: i)
    children = [worker(DummyConsumerSupervisor, [])]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule DummyConsumer do
  @moduledoc false
  use Nostrum.Consumer

  require Logger

  def start_link do
    Consumer.start_link(__MODULE__, [])
  end

  def handle_event({event_name, _, _}, _) do
    Logger.debug("User would handle #{event_name} here")
    {:ok, %{}}
  end
end

defmodule DummyConsumerSupervisor do
  use Nostrum.TaskedConsumer

  require Logger

  def start_link do
    TaskedConsumer.start_link(__MODULE__)
  end

  def handle_event({:MESSAGE_CREATE, {message}, _}) do
    Logger.debug("Message received: #{inspect(message.content)}")
  end

  def handle_event({event_name, _, _}) do
    Logger.debug("User would handle #{event_name} here")
  end
end
