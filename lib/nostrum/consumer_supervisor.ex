defmodule Nostrum.ConsumerSupervisor do
  @moduledoc """
  ConsumerSupervisor for gateway event handling.
  # Differences against Consumer
  Unlike Consumer, This module will start up new Tasks to handle
  events, allowing for longer running handles that don't block the entire
  consumer. There's a limit on the amount of concurrent Tasks, so
  it isn't possible to overload the VM in this module.
  ## Event specification
  See the documentation in `Nostrum.Consumer` for more information
  about events.
  ## Example usage
  ```elixir
  defmodule ExampleConSup do
    use Nostrum.ConsumerSupervisor
    alias Nostrum.Api

    def handle_event({:MESSAGE_CREATE, {msg}, _ws_state}) do
      case msg.content do
        "!sleep" ->
           Api.create_message(msg.channel_id, "Going to sleep...")
           # This won't stop other events from being handled
           Process.sleep(3000)
        "!ping" ->
          Api.create_message(msg.channel_id, "pong!")
        "!raise" ->
          # This won't crash the entire Consumer
          raise "This will be just fine!"
        _ ->
         :ignore
      end
    end

    def handle_event(_event) do
    end
  end
  """

  use ConsumerSupervisor

  @callback handle_event(Nostrum.Consumer.event) :: any

  defmacro __using__(_) do
    quote location: :keep do
      @module __MODULE__
      @behaviour Nostrum.ConsumerSupervisor
      alias Nostrum.ConsumerSupervisor

      def handle_event(_event) do
        :ok
      end

      contents = quote do
        def start_link(event) do
          Task.start_link fn ->
            unquote(@module).handle_event(event)
          end
        end
      end

      Module.create(__MODULE__.Runner, contents, Macro.Env.location(__ENV__))

      defoverridable [handle_event: 1]
    end
  end

  def start_link(mod) do
    ConsumerSupervisor.start_link(__MODULE__, {:mod, mod})
  end

  @doc false
  def init({:mod, mod}) do
    producers =
      ProducerRegistry
      |> Registry.lookup(:pids)
      |> Enum.map(fn {pid, _value} -> pid end)
    children = [worker(Module.concat(mod, Runner), [], restart: :temporary)]
    {:ok, children, strategy: :one_for_one, subscribe_to: producers}
  end
end
