defmodule ExampleSupervisor do
  def start do
    import Supervisor.Spec

    # List comprehension creates a consumer per cpu core
    children = for i <- 1..System.schedulers_online, do: worker(ExampleConsumer, [], id: i),

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule ExampleConsumer do
  use Mixcord.Shard.Dispatch.Consumer
  require Logger

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  def handle_event({{:MESSAGE_CREATE, msg}, ws_state}, state) do
    def handle_message_create(msg) do
      case msg.content do
        <<"!" :: binary, "ping" :: binary>> ->
          Api.create_message(msg.channel.id, "I copy and pasted this code")
      end
    end
    {:ok, state}
  end
end
