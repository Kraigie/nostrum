defmodule Nostrum.Cache.PresenceCacheTest do
  use ExUnit.Case

  @cache_modules [
    # Implementations
    Nostrum.Cache.PresenceCache.ETS
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

      describe "empty cache" do
        setup do
          [pid: start_supervised!(@cache)]
        end

        test "lookup of nonexistent user" do
          assert {:error, :presence_not_found} =
                   @cache.get(@test_presence.user.id + 1, @test_guild_id)
        end

        test "create, update, delete" do
          # Used for pinning.
          test_presence = @test_presence
          test_guild_id = @test_guild_id

          assert :ok = @cache.create(@test_presence)
          assert :noop = @cache.update(@test_presence)
          assert {:ok, ^test_presence} = @cache.get(@test_presence.user.id, @test_guild_id)

          updated_presence = Map.put(@test_presence, :status, :offline)

          assert {^test_guild_id, ^test_presence, ^updated_presence} =
                   @cache.update(updated_presence)
        end
      end
    end
  end
end
