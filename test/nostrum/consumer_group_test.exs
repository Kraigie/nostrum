defmodule Nostrum.ConsumerGroupTest do
  import Nostrum.Bot, only: [set_bot_name: 1, with_bot: 2]
  alias Nostrum.ConsumerGroup
  alias Nostrum.Shard.Dispatch
  alias Nostrum.Struct.Message
  alias Nostrum.Struct.WSState
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
      message_2 = %Message{content: "greg's gat"}

      events = [
        {:MESSAGE_CREATE, message, %WSState{}},
        {:MESSAGE_CREATE, message_2, %WSState{}}
      ]

      ^events = ConsumerGroup.dispatch(events, bot_name)

      assert_receive {:event, {:MESSAGE_CREATE, ^message, _dummy_ws_state}}
      assert_receive {:event, {:MESSAGE_CREATE, ^message_2, _dummy_ws_state}}
    end

    # :noop events are explicitly filtered out before being dispatched to ConsumerGroup
    test "Raises on dispatching `malformed events` messages", %{bot_name: bot_name} do
      assert_raise FunctionClauseError, fn ->
        ConsumerGroup.dispatch(:noop, bot_name)
      end
    end

    test "Nostrum.Shard.Dispatch.handle/2 forwards messages to subscriber", %{bot_name: bot_name} do
      interaction = %{guild_id: guild_id = 19_411_207, channel_id: channel_id = 20_010_911}
      state = %WSState{bot_options: %{name: bot_name, consumer: NostrumTest.NoopConsumer}}
      payload = %{t: :INTERACTION_CREATE, d: interaction}

      [
        {:INTERACTION_CREATE,
         %Nostrum.Struct.Interaction{guild_id: ^guild_id, channel_id: ^channel_id},
         ^state} = event
      ] = Dispatch.handle(payload, state)

      assert_receive {:event, ^event}
    end

    test "Nostrum.Shard.Dispatch.handle/2 filters noops from consumers", %{bot_name: bot_name} do
      Logger.disable(self())
      payload = %{t: :UNMATCHED_EVENT_NAME, d: :noop}
      state = %WSState{bot_options: %{name: bot_name, consumer: NostrumTest.NoopConsumer}}

      [] = Dispatch.handle(payload, state)

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
