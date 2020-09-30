defmodule EventConsumerTest do
  use ExUnit.Case
  doctest EventConsumer

  test "greets the world" do
    assert EventConsumer.hello() == :world
  end
end
