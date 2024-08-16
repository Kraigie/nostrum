defmodule Nostrum.Cache.UserCacheMetaTest do
  alias Nostrum.Cache.UserCache
  use ExUnit.Case, async: true

  @cache_modules [
    # Dispatcher
    UserCache,
    # Implementations
    UserCache.ETS,
    UserCache.Mnesia
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      alias Nostrum.Struct.User
      use ExUnit.Case

      # this is needed because otherwise we cannot access
      # the cache in the tests
      @cache cache
      @test_user %{
        id: 12345,
        username: "test",
        discriminator: "1234",
        avatar: nil,
        bot: true
      }
      @test_user_two %{
        id: 54321,
        username: "test two",
        discriminator: "0",
        global_name: "test_two",
        avatar: nil,
        bot: true
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

      describe "bulk_create/1" do
        test "returns `:ok`" do
          users = [@test_user, @test_user_two]
          assert :ok = @cache.bulk_create(users)
        end
      end

      describe "create/1" do
        test "returns a struct of the created user" do
          expected = User.to_struct(@test_user)

          assert ^expected = @cache.create(@test_user)
        end
      end

      describe "delete/1" do
        test "returns `:noop` on uncached user" do
          assert :noop = @cache.delete(10_258_109_258_109_258_125)
        end

        test "returns user struct on cached user" do
          expected = User.to_struct(@test_user)
          @cache.create(@test_user)
          assert ^expected = @cache.delete(@test_user.id)
        end
      end

      describe "get/1" do
        test "returns error tuple on unknown user" do
          assert {:error, _reason} = @cache.get(120_815_092_581_902_580)
        end

        test "returns cached user on known user" do
          expected = User.to_struct(@test_user)
          @cache.create(@test_user)
          assert {:ok, ^expected} = @cache.get(@test_user.id)
        end
      end

      describe "update/1" do
        test "returns `{nil, after}` on uncached user" do
          payload = %{id: 8_284_967_893_178_597}
          expected = {nil, User.to_struct(payload)}
          assert ^expected = @cache.update(payload)
        end

        test "returns `{before, after}` on cached user" do
          expected_before = User.to_struct(@test_user)
          updated_test_user = Map.put(@test_user, :username, "updated test user")
          expected_after = User.to_struct(updated_test_user)
          @cache.create(@test_user)

          assert {^expected_before, ^expected_after} = @cache.update(updated_test_user)
        end
      end
    end
  end
end
