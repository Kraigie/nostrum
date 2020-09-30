# The event consumer that will be handling all incoming events.
defmodule EventConsumer.ConsumerWithCache do
  use Nostrum.Consumer

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  # We only need to write event handlers for the events we are interested in,
  # the rest will go to the catch-all case to be ignored.
  def handle_event({:MESSAGE_CREATE, message, _ws_state}) do
    EventConsumer.command(message)
  end

  # The catch-all event case that takes the rest of the events.
  # If you do not have this, or have not defined literally
  # every event case yourself (what an absolute madlad if you have),
  # the consumer will crash, not having any function signature to match upon.
  # By the way, the return atom stands for "no-op", shorthand for "no operation",
  # however, if you read it as "noop" and quietly chuckle to yourself every time you see it,
  # since it just sounds like a silly way of saying "nope", know, that you are not alone.
  def handle_event(_event), do: :noop
end
