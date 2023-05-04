_ = """
  This module exists primarily for documentation/testing purposes
"""

defmodule DummySupervisor do
  @moduledoc false

  use Supervisor

  def start_link(_args) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_args) do
    children = for id <- 0..1, do: Supervisor.child_spec({DummyConsumer, []}, id: id)
    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule DummyConsumer do
  @moduledoc false
  use Nostrum.Consumer

  require Logger

  def handle_event({:MESSAGE_CREATE, message, _}) do
    Logger.debug(fn -> "Message received: #{inspect(message.content)}" end)
  end

  def handle_event({event_name, _, _}) do
    Logger.debug(fn -> "User would handle #{event_name} here" end)
  end
end
