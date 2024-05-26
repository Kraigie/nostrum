defmodule Nostrum.Cache.MessageCache.MnesiaAdditionalTest do
  use ExUnit.Case

  alias Nostrum.Cache.MessageCache
  alias Nostrum.Struct.Message

  @test_message %{
    id: 1_234_567_000,
    channel_id: 7_654_321_000,
    author: %{
      id: 12_345_000,
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
    id: 7_654_321_000_000_000,
    channel_id: 1_234_567_000,
    author: %{
      id: 54_321_000,
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
        MessageCache.Mnesia.teardown()
      rescue
        e -> e
      end
    end)

    [pid: start_supervised!(MessageCache.Mnesia)]
  end

  describe "create/1" do
    test "evicts the messages with the lowest ids when it gets full" do
      for id <- 1..11, do: MessageCache.Mnesia.create(Map.put(@test_message, :id, id))

      # in tests, the cache is limited to 10 messages
      # and we evict 4 messages when hitting the limit
      # so the first 4 messages should be evicted

      for id <- 1..4 do
        assert MessageCache.Mnesia.get(id) == {:error, :not_found}
      end

      for id <- 5..11 do
        assert {:ok, %Message{id: ^id}} = MessageCache.Mnesia.get(id)
      end
    end

    test "eviction for tables of type set works as well" do
      # drop and recreate the table with a different type
      MessageCache.Mnesia.teardown()

      table_create_attributes =
        MessageCache.Mnesia.table_create_attributes()
        |> Keyword.put(:type, :set)

      {:atomic, :ok} = :mnesia.create_table(MessageCache.Mnesia.table(), table_create_attributes)

      for id <- 1..11, do: MessageCache.Mnesia.create(Map.put(@test_message, :id, id))

      for id <- 1..4 do
        assert MessageCache.Mnesia.get(id) == {:error, :not_found}
      end

      for id <- 5..11 do
        assert {:ok, %Message{id: ^id}} = MessageCache.Mnesia.get(id)
      end
    end
  end

  describe "update/1" do
    test "returns {old_message, updated_message} when the old message is found in the cache" do
      expected_old_message = MessageCache.Mnesia.create(@test_message_two)

      updated_payload = %{
        id: @test_message_two.id,
        content: "Hello, world!",
        channel_id: @test_message_two.channel_id
      }

      {old_message, updated_message} = MessageCache.Mnesia.update(updated_payload)

      assert old_message == expected_old_message
      assert updated_message == %{old_message | content: "Hello, world!"}
    end

    test "does not save the updated message to the cache it was not there before" do
      updated_payload = %{
        id: 10_258_109_258_109_258_125,
        content: "Hello, world!",
        channel_id: 10_258_109_258_109_258_125
      }

      {old_message, updated_message} = MessageCache.Mnesia.update(updated_payload)

      assert updated_message == Message.to_struct(updated_payload)
      assert old_message == nil
      assert MessageCache.Mnesia.get(10_258_109_258_109_258_125) == {:error, :not_found}
    end
  end

  describe "get/1" do
    test "returns {:ok, message} when the message is found in the cache" do
      expected = MessageCache.Mnesia.create(@test_message)
      assert {:ok, expected} == MessageCache.Mnesia.get(@test_message.id)
    end
  end

  describe "delete/2" do
    test "returns the deleted message when it is found in the cache" do
      expected_message = MessageCache.Mnesia.create(@test_message)

      assert expected_message ==
               MessageCache.Mnesia.delete(@test_message.channel_id, @test_message.id)
    end
  end

  describe "bulk_delete/2" do
    test "returns the deleted messages when they are found in the cache" do
      expected_messages = [
        MessageCache.Mnesia.create(@test_message),
        MessageCache.Mnesia.create(%{@test_message_two | channel_id: @test_message.channel_id})
      ]

      assert expected_messages ==
               MessageCache.Mnesia.bulk_delete(@test_message.channel_id, [
                 @test_message.id,
                 @test_message_two.id
               ])
    end

    test "does not include messages not found in the cache in the returned list" do
      expected_message = MessageCache.Mnesia.create(@test_message)

      assert [expected_message] ==
               MessageCache.Mnesia.bulk_delete(@test_message.channel_id, [
                 @test_message.id,
                 @test_message_two.id
               ])
    end
  end

  describe "channel_delete/1" do
    test "deletes all messages for the channel" do
      MessageCache.Mnesia.create(@test_message)
      MessageCache.Mnesia.create(%{@test_message_two | channel_id: @test_message.channel_id})

      assert :ok == MessageCache.Mnesia.channel_delete(@test_message.channel_id)
      assert {:error, :not_found} == MessageCache.Mnesia.get(@test_message.id)
      assert {:error, :not_found} == MessageCache.Mnesia.get(@test_message_two.id)
    end
  end

  describe "get_by_channel/4" do
    setup do
      MessageCache.Mnesia.create(@test_message)
      MessageCache.Mnesia.create(%{@test_message_two | channel_id: @test_message.channel_id})
      MessageCache.Mnesia.create(%{@test_message_two | id: 10_000_000_000_000_000})

      :ok
    end

    test "only returns messages with the given channel_id" do
      expected_message_one = Message.to_struct(@test_message)

      expected_message_two =
        Message.to_struct(%{@test_message_two | channel_id: @test_message.channel_id})

      assert [expected_message_one, expected_message_two] ==
               MessageCache.get_by_channel(
                 @test_message.channel_id,
                 0,
                 :infinity,
                 MessageCache.Mnesia
               )
    end

    test "it allows constraining the messages by timestamp" do
      expected_message_one = Message.to_struct(@test_message)

      assert [expected_message_one] ==
               MessageCache.get_by_channel(
                 @test_message.channel_id,
                 0,
                 5_000_000_000_000,
                 MessageCache.Mnesia
               )
    end

    test "it also allows giving a datetime instead of a snowflake" do
      expected_message_one = Message.to_struct(@test_message)

      assert [expected_message_one] ==
               MessageCache.get_by_channel(
                 @test_message.channel_id,
                 ~U[2015-01-01 00:00:00.0000Z],
                 ~U[2015-01-02 00:00:00.0000Z],
                 MessageCache.Mnesia
               )
    end
  end

  describe "get_by_author/4" do
    setup do
      MessageCache.Mnesia.create(@test_message)
      MessageCache.Mnesia.create(@test_message_two)

      MessageCache.Mnesia.create(%{
        @test_message_two
        | id: 10_000_000_000_000,
          channel_id: 500_000_000
      })

      :ok
    end

    test "only returns messages with the given author_id" do
      expected_message_one =
        Message.to_struct(%{@test_message_two | id: 10_000_000_000_000, channel_id: 500_000_000})

      expected_message_two = Message.to_struct(@test_message_two)

      assert [expected_message_one, expected_message_two] ==
               MessageCache.get_by_author(
                 @test_message_two.author.id,
                 0,
                 :infinity,
                 MessageCache.Mnesia
               )
    end

    test "it allows constraining the messages by timestamp" do
      expected_message_one = Message.to_struct(@test_message_two)

      assert [expected_message_one] ==
               MessageCache.get_by_author(
                 @test_message_two.author.id,
                 5_000_000_000_000_000,
                 11_000_000_000_000_000,
                 MessageCache.Mnesia
               )
    end

    test "it also allows giving a datetime instead of a snowflake" do
      expected_message_one = Message.to_struct(@test_message_two)

      assert [expected_message_one] ==
               MessageCache.get_by_author(
                 @test_message_two.author.id,
                 ~U[2015-01-20 00:00:00.0000Z],
                 ~U[2015-01-25 00:00:00.0000Z],
                 MessageCache.Mnesia
               )
    end
  end

  describe "get_by_channel_id_and_author/5" do
    setup do
      MessageCache.Mnesia.create(@test_message)
      MessageCache.Mnesia.create(%{@test_message_two | channel_id: @test_message.channel_id})
      MessageCache.Mnesia.create(%{@test_message_two | id: 10_000_000_000_000})

      :ok
    end

    test "only returns messages with the given channel_id and author_id" do
      expected_message_one = Message.to_struct(@test_message)

      assert [expected_message_one] ==
               MessageCache.get_by_channel_and_author(
                 @test_message.channel_id,
                 @test_message.author.id,
                 0,
                 :infinity,
                 MessageCache.Mnesia
               )
    end

    test "it allows constraining the messages by timestamp" do
      expected_message_one = Message.to_struct(@test_message)

      assert [expected_message_one] ==
               MessageCache.get_by_channel_and_author(
                 @test_message.channel_id,
                 @test_message.author.id,
                 0,
                 5_000_000_000_000,
                 MessageCache.Mnesia
               )
    end

    test "it also allows giving a datetime instead of a snowflake" do
      expected_message_one = Message.to_struct(@test_message)

      assert [expected_message_one] ==
               MessageCache.get_by_channel_and_author(
                 @test_message.channel_id,
                 @test_message.author.id,
                 ~U[2015-01-01 00:00:00.0000Z],
                 ~U[2015-01-02 00:00:00.0000Z],
                 MessageCache.Mnesia
               )
    end
  end
end
