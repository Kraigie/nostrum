defmodule Nostrum.Cache.DMChannelCacheMetaTest do
  alias Nostrum.Cache.DMChannelCache
  alias Nostrum.Struct.Channel
  use ExUnit.Case, async: true

  @cache_modules [
    # Dispatchers
    DMChannelCache,
    # Implementations
    DMChannelCache.ETS,
    DMChannelCache.Mnesia
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      use ExUnit.Case
      # this is needed because otherwise we cannot access
      # the cache in the tests
      @cache cache
      @test_channel %{
        id: 91231,
        name: "Joe's Grumblings"
      }

      doctest @cache

      describe "with an empty cache" do
        setup do
          if function_exported?(@cache, :teardown, 0) do
            on_exit(:cleanup, fn -> apply(@cache, :teardown, []) end)
          end

          [pid: start_supervised!(@cache)]
        end

        test "get/1 returns channel not found" do
          assert {:error, :not_found} = DMChannelCache.get(123, @cache)
        end

        test "create/1 returns channel" do
          expected = Channel.to_struct(@test_channel)
          assert ^expected = @cache.create(@test_channel)
        end

        test "update/1 with a non-existing channel returns :noop" do
          updated_channel = %{@test_channel | id: @test_channel.id + 1}
          expected = {nil, Channel.to_struct(updated_channel)}
          assert ^expected = @cache.update(updated_channel)
        end
      end

      describe "with cached channel" do
        setup do
          pid = start_supervised!(@cache)

          if function_exported?(@cache, :teardown, 0) do
            on_exit(:cleanup, fn -> apply(@cache, :teardown, []) end)
          end

          @cache.create(@test_channel)
          [pid: pid]
        end

        test "get/1 returns channel" do
          expected = Channel.to_struct(@test_channel)
          assert {:ok, ^expected} = DMChannelCache.get(@test_channel.id, @cache)
        end

        test "update/1 updates a channel" do
          updated_channel = Map.put(@test_channel, :name, "Craig's Grumbling")
          expected = Channel.to_struct(updated_channel)

          {old, new} = @cache.update(updated_channel)
          old_name = @test_channel.name

          assert old_name == old.name
          assert expected == new
        end

        test "delete/1 deletes a channel the channel deleted" do
          deleted = @cache.delete(@test_channel.id)
          expected = Channel.to_struct(@test_channel)
          assert deleted == expected
          assert {:error, :not_found} = DMChannelCache.get(@test_channel.id, @cache)

          # double delete:
          assert :noop == @cache.delete(@test_channel.id)
        end
      end
    end
  end
end
