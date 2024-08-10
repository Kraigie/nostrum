defmodule Nostrum.Cache.PresenceCacheMetaTest do
  alias Nostrum.Cache.PresenceCache
  use ExUnit.Case, async: true

  @cache_modules [
    # Dispatcher
    PresenceCache,
    # Implementations
    PresenceCache.ETS,
    PresenceCache.Mnesia
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      use ExUnit.Case
      # this is needed because otherwise we cannot access
      # the cache in the tests
      @cache cache
      @test_guild_id 12_039_152
      @test_presence %{
        activities: [
          %{
            created_at: 1_633_974_786_695,
            id: "custom",
            name: "Custom Status",
            state: "#OpenErlang",
            type: 4
          }
        ],
        client_status: %{desktop: :online},
        game: %{
          created_at: 1_633_974_786_695,
          id: "custom",
          name: "Custom Status",
          state: "#OpenErlang",
          type: 4
        },
        guild_id: @test_guild_id,
        status: :online,
        user: %{id: 12_049_182_940}
      }

      doctest @cache

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

      describe "empty cache" do
        test "lookup of nonexistent user" do
          assert {:error, :presence_not_found} =
                   @cache.get(@test_guild_id, @test_presence.user.id + 1)
        end

        test "create, update, delete" do
          # Used for pinning.
          test_presence = @test_presence
          test_guild_id = @test_guild_id

          assert :ok = @cache.create(@test_presence)
          assert :noop = @cache.update(@test_presence)

          assert {:ok, ^test_presence} =
                   @cache.get(@test_guild_id, test_presence.user.id)

          updated_presence = Map.put(@test_presence, :status, :offline)

          assert {^test_guild_id, ^test_presence, ^updated_presence} =
                   @cache.update(updated_presence)
        end

        test "bulk_create/2 with two presences" do
          guild_id = :erlang.unique_integer([:positive])
          user_1_id = :erlang.unique_integer([:positive])
          user_2_id = :erlang.unique_integer([:positive])
          user_1_presence = %{status: :online, user: %{id: user_1_id}}
          user_2_presence = %{status: :offline, user: %{id: user_2_id}}
          presences = [user_1_presence, user_2_presence]
          assert :ok = @cache.bulk_create(guild_id, presences)
          assert {:ok, ^user_1_presence} = @cache.get(guild_id, user_1_id)
          assert {:ok, ^user_2_presence} = @cache.get(guild_id, user_2_id)
        end

        test "bulk_create/2 with empty presences" do
          guild_id = :erlang.unique_integer([:positive])
          assert :ok = @cache.bulk_create(guild_id, [])
        end
      end
    end
  end
end
