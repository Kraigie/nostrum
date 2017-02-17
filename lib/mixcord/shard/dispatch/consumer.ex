defmodule Mixcord.Shard.Dispatch.Consumer do
  @moduledoc false

  use GenStage
  alias Mixcord.Producer

  @doc """
  Callback used to handle events.

  `Event` is the event name as an atom, and `ws_state` is the current state of
  the websocket that the event was received on. For more information on this please
  see `Mixcord.Shard.Payload.state_map.t`.

  `from` is the process information of the producer from which the demand was received.
  `state` is the internal state of your consumer.
  """
  @callback handle_event(Producer.Events.event) :: {:ok, Map.t}

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour Mixcord.Shard.Dispatch.Consumer

      def handle_event(_event, state) do
        {:ok, state}
      end

      defoverridable [handle_event: 2]
    end
  end

  def start_link(mod) do
    GenStage.start_link(__MODULE__, %{mod: mod, state: %{}})
  end

  def init(state) do
    # TODO: Attach to all producers
    {:consumer, state, subscribe_to: [Mixcord.Shard.Dispatch.Producer]}
  end

  def handle_events(events, _from, %{mod: mod, state: their_state} = state) do
    for event <- events do
      # TODO: Put user state in our state
      {:ok, their_state_ret} = mod.handle_event(event, their_state)
    end
    {:noreply, [], state}
    # {:noreply, [], %{state | state: their_state_ret}}
  end

end
