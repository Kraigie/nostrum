defmodule Nostrum.Cache.MessageCacheMetaTest do
  alias Nostrum.Cache.MessageCache
  use ExUnit.Case, async: true

  @cache_modules [
    # Dispatcher
    MessageCache,
    # Implementation
    MessageCache.Mnesia,
    MessageCache.Noop
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      alias Nostrum.Struct.Message
      use ExUnit.Case

      # this is needed because otherwise we cannot access
      # the cache in the tests
      @cache cache

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
            timestamp: 0,
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
            if function_exported?(@cache, :teardown, 0) do
              apply(@cache, :teardown, [])
            end
          rescue
            e -> e
          end
        end)

        [pid: start_supervised!(@cache)]
      end

      describe "create/1" do
        test "returns a struct of the created message" do
          expected_one = Message.to_struct(@test_message)
          assert expected_one == @cache.create(@test_message)

          expected_two = Message.to_struct(@test_message_two)
          assert expected_two == @cache.create(@test_message_two)
        end
      end

      describe "update/1" do
        test "returns {nil, updated_message} on and uncached message" do
          expected = Message.to_struct(@test_message)
          assert {nil, expected} == @cache.update(@test_message)
        end
      end

      describe "get/1" do
        test "returns {:error, :not_found} on an uncached message" do
          assert {:error, :not_found} == @cache.get(10_258_109_258_109_258_125)
        end
      end

      describe "delete/2" do
        test "returns nil on an uncached message" do
          assert nil == @cache.delete(10_258_109_258_109_258_125, 10_258_109_258_109_258_125)
        end
      end
    end
  end
end
