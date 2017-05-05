defmodule Nostrum.Shard.Stage.Cache do
  @moduledoc false

  use GenStage

  alias Nostrum.Shard.Dispatch

  require Logger

  def start_link(producer) do
    GenStage.start_link(__MODULE__, producer)
  end

  def init(producer) do
    Registry.register(CacheStageRegistry, :pids, self())
    {:producer_consumer, [], subscribe_to: [producer]}
  end

  def handle_events(events, _from, state) do
    events = Enum.map(events, &Dispatch.handle/1)
    {:noreply, events, state}
  end
end
