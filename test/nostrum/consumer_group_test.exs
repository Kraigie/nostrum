defmodule Nostrum.ConsumerGroupTest do
  alias Nostrum.ConsumerGroup
  alias Nostrum.Struct.Message
  use ExUnit.Case, async: true

  doctest ConsumerGroup

  setup_all do
    [pid: start_supervised!(ConsumerGroup)]
  end

  describe "inline awaiting" do
    setup do
      :ok = ConsumerGroup.join()
    end

    test "forwards messages to the subscriber" do
      message = %Message{content: "craig's cat"}
      :ok = ConsumerGroup.dispatch({:MESSAGE_CREATE, message})
      assert_receive {:event, {:MESSAGE_CREATE, ^message}}
    end

    test "does not forward single `:noop` messages" do
      :ok = ConsumerGroup.dispatch(:noop)
      refute_receive _
    end

    test "does not forward `:noop` messages in a list of events" do
      :ok = ConsumerGroup.dispatch([:noop])
      refute_receive _
    end
  end

  describe "monitoring" do
    test "informs clients about changes to the group" do
      {ref, [] = _members} = ConsumerGroup.monitor()
      parent = self()

      pid =
        spawn(fn ->
          ConsumerGroup.join()
          send(parent, :ready)
        end)

      assert_receive :ready
      assert_receive {^ref, :join, _group, [^pid]}
      assert_receive {^ref, :leave, _group, [^pid]}
      :ok = ConsumerGroup.demonitor(ref)
    end
  end
end
