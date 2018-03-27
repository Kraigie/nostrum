defmodule Nostrum.TaskedConsumer do
  @moduledoc """
  ConsumerSupervisor for gateway event handling.

  # Consumer vs TaskedConsumer
  Unlike `Nostrum.Consumer`, this module will start up a new task to handle
  each event, allowing for longer running handles that don't block the entire
  consumer.

  There's a limit on the amount of concurrently running tasks. This module acts
  as a pool of tasks to prevent any overloading while still benefiting from

  ## Event specification
  See the documentation in `Nostrum.Consumer` for more information
  about events.

  ## Example
  An example consumer can be found
  [here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer_supervisor.ex).
  """

  use ConsumerSupervisor

  @callback handle_event(Nostrum.Consumer.event()) :: any

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Nostrum.TaskedConsumer

      use Task

      alias Nostrum.TaskedConsumer

      def handle_event(_event) do
        :ok
      end

      # REVIEW: Work around appending arguments (not possible? first line here: 
      # https://hexdocs.pm/gen_stage/ConsumerSupervisor.html#c:init/1)
      def start_link([], event) do
        Task.start_link(fn ->
          __MODULE__.handle_event(event)
        end)
      end

      defoverridable handle_event: 1
    end
  end

  def start_link(mod) do
    ConsumerSupervisor.start_link(__MODULE__, [mod])
  end

  @doc false
  def init([mod]) do
    producers =
      CacheStageRegistry
      |> Registry.lookup(:pids)
      |> Enum.map(fn {pid, _value} -> pid end)

    children = [
      Supervisor.child_spec(mod, [])
    ]

    {:ok, children, strategy: :one_for_one, subscribe_to: producers}
  end
end
