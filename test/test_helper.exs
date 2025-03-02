ExUnit.configure(exclude: [disabled: true])
:mnesia.start()
ExUnit.start()

defmodule NostrumTest.NoopConsumer do
  @behaviour Nostrum.Consumer
  def handle_event(_), do: :noop
end
