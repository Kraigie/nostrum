defmodule ExampleSupervisor do
  def start do
    import Supervisor.Spec

    # List comprehension creates a consumer per cpu core
    children = for i <- 1..System.schedulers_online, do: worker(ExampleConsumer, [], id: i),

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule ExampleConsumer do

  use GenStage
  alias Mixcord.Api

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  # Subscribe to Mixcord's event dispatcher upon startup
  def init(:ok) do
    {:consumer, :ok, subscribe_to: [Mixcord.Shard.Dispatch.Producer]}
  end

  def handle_events(events, _from, state) do
    for event <- events do
      with
        {{event_info}, state} <- event,
        {event_name, _payload} <- event_info
        # Just handling message_create
        {:MESSAGE_CREATE, msg, _state} <- event_info
      do
        handle_message_create(msg)
      else
        Logger.info "Not handling event #{event_name}"
      end
    end
    {:noreply, [], state}
  end

  def handle_message_create(msg) do
    case msg.content do
      <<"!" :: binary, "ping" :: binary>> ->
        Api.create_message(msg.channel.id, "I copy and pasted this code")
    end
  end
end
