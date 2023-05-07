defmodule Nostrum.Cache.ChannelCacheTest do
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Message
  alias Nostrum.Application
  use ExUnit.Case

  @cache_modules [
    # Implementations
    Nostrum.Cache.ChannelCache.ETS
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
          Application.setup_ets_tables()
          [pid: start_supervised!(@cache)]
        end

        test "get/1 returns channel not found" do
          assert @cache.get(123) == {:error, :channel_not_found}
        end

        test "create/1 returns true" do
          expected = Channel.to_struct(@test_channel)
          assert @cache.create(@test_channel) == expected
        end

        test "update/1 with a non-existing channel returns :noop" do
          assert @cache.update(@test_channel) == :noop
        end
      end

      describe "with cached channel" do
        setup do
          Application.setup_ets_tables()
          pid = start_supervised!(@cache)
          @cache.create(@test_channel)
          [pid: pid]
        end

        test "get/1 on dispatch module with message returns channel" do
          expected = Channel.to_struct(@test_channel)

          assert Nostrum.Cache.ChannelCache.get(%Message{channel_id: @test_channel.id}) ==
                   {:ok, expected}
        end

        test "get/1 returns channel" do
          expected = Channel.to_struct(@test_channel)
          assert @cache.get(@test_channel.id) == {:ok, expected}
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
          assert @cache.get(@test_channel.id) == {:error, :channel_not_found}

          # double delete:
          assert :noop == @cache.delete(@test_channel.id)
        end
      end
    end
  end
end
