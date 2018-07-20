defmodule Nostrum.Shard.Stage.Cache do
  @moduledoc false

  use GenStage

  alias Nostrum.Shard.Dispatch
  alias Nostrum.Shard.Stage.Producer

  require Logger

  def start_link(opts) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    {:producer_consumer, [], subscribe_to: [Producer]}
  end

  def handle_events(events, _from, state) do
    flat_processed_events =
      events
      |> Task.async_stream(&Dispatch.handle/1)
      |> Stream.map(fn {:ok, ret} -> ret end)
      |> Enum.to_list()
      |> List.flatten()
      |> Enum.filter(fn event -> event != :noop end)

    {:noreply, flat_processed_events, state}
  end

end
