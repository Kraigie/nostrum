defmodule :nostrum_noop_consumer do
  @behaviour Nostrum.Consumer
  def handle_event(_), do: :noop
end
