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
    flat_processed_events =
      events
      |> Task.async_stream(&Dispatch.handle/1)
      |> Enum.map(fn {:ok, ret} -> ret end)
      |> Enum.to_list
      |> List.flatten
      |> remove_noop
    {:noreply, flat_processed_events, state}
  end

  defp remove_noop(events) do
    Enum.filter(events, fn event -> event != :noop end) 
  end
end
