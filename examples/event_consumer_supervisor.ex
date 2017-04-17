defmodule ExampleSupervisor do
  def start do
    import Supervisor.Spec

    children = children = [worker(ExampleTaskedConsumer, [])]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule ExampleTaskedConsumer do
  use Nostrum.TaskedConsumer
  alias Nostrum.Api

  def handle_event({:MESSAGE_CREATE, {msg}, _ws_state}) do
    case msg.content do
      "!sleep" ->
         Api.create_message(msg.channel_id, "Going to sleep...")
         # This won't stop other events from being handled
         Process.sleep(3000)
      "!ping" ->
        Api.create_message(msg.channel_id, "pyongyang!")
      "!raise" ->
        # This won't crash the entire Consumer
        raise "No problems here!"
      _ ->
       :ignore
    end
  end

  def handle_event(_event) do
    :noop
  end
end
