defmodule Nostrum.ConsumerGroupTest do
  import Nostrum.Bot, only: [set_bot_name: 1, with_bot: 2]
  alias Nostrum.ConsumerGroup
  alias Nostrum.Struct.Message
  use ExUnit.Case, async: true

  doctest ConsumerGroup

  setup_all do
    bot_name = :consumer_group_test

    bot_options = %{
      name: bot_name
    }

    spec = {ConsumerGroup, bot_options}
    [pid: start_supervised!(spec), bot_name: bot_name]
  end

  setup %{bot_name: bot_name} do
    set_bot_name(bot_name)
    :ok
  end

  describe "inline awaiting" do
    setup %{bot_name: bot_name} do
      set_bot_name(bot_name)
      :ok = ConsumerGroup.join()
    end

    test "forwards messages to the subscriber", %{bot_name: bot_name} do
      message = %Message{content: "craig's cat"}
      :ok = ConsumerGroup.dispatch({:MESSAGE_CREATE, message}, bot_name)
      assert_receive {:event, {:MESSAGE_CREATE, ^message}}
    end

    test "does not forward single `:noop` messages", %{bot_name: bot_name} do
      :ok = ConsumerGroup.dispatch(:noop, bot_name)
      refute_receive _
    end

    test "does not forward `:noop` messages in a list of events", %{bot_name: bot_name} do
      :ok = ConsumerGroup.dispatch([:noop], bot_name)
      refute_receive _
    end
  end

  describe "monitoring" do
    test "informs clients about changes to the group", %{bot_name: bot_name} do
      {ref, [] = _members} = ConsumerGroup.monitor()
      parent = self()

      pid =
        spawn(fn ->
          with_bot(bot_name, fn ->
            ConsumerGroup.join()
            send(parent, :ready)
            :ok = ConsumerGroup.leave()
            :not_joined = ConsumerGroup.leave()
            :not_joined = ConsumerGroup.leave(self())
          end)
        end)

      assert_receive :ready
      assert_receive {^ref, :join, _group, [^pid]}
      assert_receive {^ref, :leave, _group, [^pid]}
      :ok = ConsumerGroup.demonitor(ref)
    end
  end
end
