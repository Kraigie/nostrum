defmodule ExampleSupervisor do
  def start do
    import Supervisor.Spec

    children = [
      worker(ExampleConsumer, [], id: 1),
      worker(ExampleConsumer, [], id: 2),
      worker(ExampleConsumer, [], id: 3),
      worker(ExampleConsumer, [], id: 4)
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule ExampleConsumer do

  use GenStage
  alias Mixcord.Api

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    {:consumer, :ok, subscribe_to: [Mixcord.Shard.Dispatch.Producer]}
  end

  def handle_message_create(msg) do
    case msg.content do
      <<"!" :: binary, "ping" :: binary>> ->
        Api.create_message(msg.channel.id, "I copy and pasted this code")
    end
  end

  def handle_events(events, _from, state) do
    for event <- events do
      with
        {{event_info}, state} <- event,
        {event_name, payload} <- event_info
        {:MESSAGE_CREATE, msg, _state} <- event_info
      do
        handle_message_create(msg)
      else
        Logger.info "Not handling event #{event_name}"
      end
    end
    {:noreply, [], state}
  end
end
