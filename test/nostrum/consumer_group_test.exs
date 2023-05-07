defmodule Nostrum.ConsumerGroupTest do
  alias Nostrum.ConsumerGroup
  alias Nostrum.Struct.Message
  use ExUnit.Case

  setup_all do
    [pid: start_supervised!(ConsumerGroup)]
  end

  describe "inline awaiting" do
    test "forwards messages to the subscriber" do
      :ok = ConsumerGroup.join()
      message = %Message{content: "craig's cat"}
      :ok = ConsumerGroup.dispatch({:MESSAGE_CREATE, message})
      assert_receive {:event, {:MESSAGE_CREATE, ^message}}
    end
  end
end
