defmodule Nostrum.Cache.MessageCache.MnesiaAdditionalTest do
  use ExUnit.Case

  alias Nostrum.Cache.MessageCache.Mnesia, as: MessageCache
  alias Nostrum.Struct.Message

  @test_message %{
    id: 1_234_567,
    channel_id: 7_654_321,
    author: %{
      id: 12345,
      username: "test",
      avatar: nil,
      bot: true,
      mfa_enabled: nil,
      verified: nil
    },
    content: "Hello, world!",
    timestamp: "1970-01-01T00:00:00Z",
    edited_timestamp: nil
  }

  @test_message_two %{
    id: 7_654_321,
    channel_id: 1_234_567,
    author: %{
      id: 54321,
      username: "test two",
      avatar: nil,
      bot: false,
      mfa_enabled: nil,
      verified: nil
    },
    content: "Goodbye, world!",
    timestamp: "2038-01-01T00:00:00Z",
    edited_timestamp: nil,
    embeds: [
      %{
        title: "Test Embed",
        description: "This is a test embed",
        url: "https://example.com",
        timestamp: "2038-01-01T00:00:00Z",
        color: 0x00FF00,
        footer: %{
          text: "Test Footer"
        },
        fields: [
          %{
            name: "Test Field",
            value: "Test Value",
            inline: false
          }
        ]
      }
    ]
  }

  setup do
    on_exit(:cleanup, fn ->
      try do
        MessageCache.teardown()
      rescue
        e -> e
      end
    end)

    [pid: start_supervised!(MessageCache)]
  end

  describe "create/1" do
    test "evicts the messages with the lowest ids when it gets full" do
      for id <- 1..11, do: MessageCache.create(Map.put(@test_message, :id, id))

      # in tests, the cache is limited to 10 messages
      # and we evict 4 messages when hitting the limit
      # so the first 4 messages should be evicted

      for id <- 1..4 do
        assert MessageCache.get(id) == {:error, :not_found}
      end

      for id <- 5..11 do
        assert {:ok, %Message{id: ^id}} = MessageCache.get(id)
      end
    end
  end

  describe "update/1" do
    test "returns {old_message, updated_message} when the old message is found in the cache" do
      expected_old_message = MessageCache.create(@test_message_two)

      updated_payload = %{
        id: @test_message_two.id,
        content: "Hello, world!",
        channel_id: @test_message_two.channel_id
      }

      {old_message, updated_message} = MessageCache.update(updated_payload)

      assert old_message == expected_old_message
      assert updated_message == %{old_message | content: "Hello, world!"}
    end

    test "does not save the updated message to the cache it was not there before" do
      updated_payload = %{
        id: 10_258_109_258_109_258_125,
        content: "Hello, world!",
        channel_id: 10_258_109_258_109_258_125
      }

      {old_message, updated_message} = MessageCache.update(updated_payload)

      assert updated_message == Message.to_struct(updated_payload)
      assert old_message == nil
      assert MessageCache.get(10_258_109_258_109_258_125) == {:error, :not_found}
    end
  end

  describe "get/1" do
    test "returns {:ok, message} when the message is found in the cache" do
      expected = MessageCache.create(@test_message)
      assert {:ok, expected} == MessageCache.get(@test_message.id)
    end
  end

  describe "delete/2" do
    test "returns the deleted message when it is found in the cache" do
      expected_message = MessageCache.create(@test_message)
      assert expected_message == MessageCache.delete(@test_message.channel_id, @test_message.id)
    end
  end

  describe "bulk_delete/2" do
    test "returns the deleted messages when they are found in the cache" do
      expected_messages = [
        MessageCache.create(@test_message),
        MessageCache.create(%{@test_message_two | channel_id: @test_message.channel_id})
      ]

      assert expected_messages ==
               MessageCache.bulk_delete(@test_message.channel_id, [
                 @test_message.id,
                 @test_message_two.id
               ])
    end

    test "does not include messages not found in the cache in the returned list" do
      expected_message = MessageCache.create(@test_message)

      assert [expected_message] ==
               MessageCache.bulk_delete(@test_message.channel_id, [
                 @test_message.id,
                 @test_message_two.id
               ])
    end
  end

  describe "channel_delete/1" do
    test "deletes all messages for the channel" do
      MessageCache.create(@test_message)
      MessageCache.create(%{@test_message_two | channel_id: @test_message.channel_id})

      assert :ok == MessageCache.channel_delete(@test_message.channel_id)
      assert {:error, :not_found} == MessageCache.get(@test_message.id)
      assert {:error, :not_found} == MessageCache.get(@test_message_two.id)
    end
  end
end
