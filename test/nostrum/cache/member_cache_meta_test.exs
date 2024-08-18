defmodule Nostrum.Cache.MemberCacheMetaTest do
  alias Nostrum.Cache.MemberCache
  alias Nostrum.Cache.UserCache
  alias Nostrum.Struct.Guild.Member
  use ExUnit.Case, async: true

  @moduledoc since: "0.7.0"

  @cache_modules [
    # Dispatcher
    MemberCache,
    # Implementations
    MemberCache.ETS,
    MemberCache.Mnesia
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      use ExUnit.Case
      # this is needed because otherwise we cannot access
      # the cache in the tests
      @cache cache
      @test_guild_id 12409
      @test_member %{
        nickname: "Joe",
        roles: [],
        user_id: 120_391
      }
      @test_member_2 %{
        nickname: "Joe",
        roles: [],
        user_id: @test_member.user_id + 1
      }

      doctest @cache

      @doc """
      Collects all results from a `fold` query in the accumulator.
      """
      @doc since: "0.8.0"
      def collector(item, acc) do
        [item | acc]
      end

      setup do
        # For cache backends that persist across test cases, this is needed
        # to clean them up nicely again. Other cache backends may already
        # be stopped by then, and are `rescue`d appropriately.
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

      if function_exported?(@cache, :wrap_query, 1) do
        defdelegate wrap_query(fun), to: @cache
      else
        defp wrap_query(fun), do: fun.()
      end

      defp all_entries do
        MemberCache.fold([], @test_guild_id, fn member, acc -> [member | acc] end, @cache)
      end

      test "member management" do
        # create/2
        # the gateway sends this differently, let's change it
        {user_id, raw} = Map.pop(@test_member, :user_id)
        as_payload = Map.put(raw, :user, %{id: user_id})
        expected = Member.to_struct(as_payload)
        member = @cache.create(@test_guild_id, as_payload)
        assert ^expected = member

        guild_id = @test_guild_id
        assert [^expected] = all_entries()

        # update/2
        payload = %{as_payload | nickname: "GrumblerBot3"}
        updated = Member.to_struct(payload)
        {^guild_id, ^expected, ^updated} = @cache.update(@test_guild_id, payload)
        assert guild_id == @test_guild_id

        assert [^updated] = all_entries()

        # delete/2
        {^guild_id, ^updated} = @cache.delete(@test_guild_id, @test_member.user_id)
        assert [] = all_entries()
      end

      test "bulk_create/2" do
        # bulk_create receives the raw gateway-provided
        # member objects, which include the user.
        first_member = Map.put(@test_member, :user, %{id: @test_member.user_id})
        second_member = Map.put(@test_member_2, :user, %{id: @test_member_2.user_id})
        first_member_id = first_member.user.id
        second_member_id = second_member.user.id
        assert first_member_id != second_member_id

        chunk = [first_member, second_member]
        casted_first_member = Member.to_struct(first_member)
        casted_second_member = Member.to_struct(second_member)
        assert true = @cache.bulk_create(@test_guild_id, chunk)

        entries = Enum.sort(all_entries(), &(&1.user_id <= &2.user_id))

        assert [^casted_first_member, ^casted_second_member] = entries
      end

      describe "get_with_user/3" do
        setup do
          [user_pid: start_supervised!(UserCache)]
        end

        test "no member and no user" do
          refute MemberCache.get_with_user(1234, 12059, @cache)
        end

        test "no member but user" do
          user = %{id: :erlang.unique_integer()}
          UserCache.create(user)
          refute MemberCache.get_with_user(1234, user.id, @cache)
        end

        test "member but no user" do
          guild_id = :erlang.unique_integer()
          raw_member = %{user: %{id: :erlang.unique_integer()}}
          assert created_member = @cache.create(guild_id, raw_member)

          assert {^created_member, nil} =
                   MemberCache.get_with_user(guild_id, raw_member.user.id, @cache)
        end

        test "member with user" do
          guild_id = :erlang.unique_integer()
          raw_user = %{id: :erlang.unique_integer()}
          raw_member = %{user: raw_user}
          assert created_user = UserCache.create(raw_user)
          assert created_member = @cache.create(guild_id, raw_member)

          assert {^created_member, ^created_user} =
                   MemberCache.get_with_user(guild_id, raw_member.user.id, @cache)
        end
      end

      describe "fold_with_users/4" do
        setup do
          [user_pid: start_supervised!(UserCache)]
        end

        test "with unknown guild" do
          assert [] = MemberCache.fold_with_users([], 1_298_414, &collector/2, @cache)
        end

        test "guild with users" do
          guild_id = 123_890_195
          user_id = 128_309_125
          user = %{id: user_id, username: "joe"}
          member = %{user: user}
          assert created_user = UserCache.create(user)
          assert created_member = @cache.create(guild_id, member)

          assert [{^created_member, ^created_user}] =
                   MemberCache.fold_with_users([], guild_id, &collector/2, @cache)
        end
      end

      describe "fold_by_user/4" do
        setup do
          [user_pid: start_supervised!(UserCache)]
        end

        test "with unknown guild" do
          assert [] = MemberCache.fold_by_user([], 1_298_414, &collector/2, @cache)
        end

        test "guild with users" do
          guild_id = 123_890_195
          user_id = 128_309_125
          user = %{id: user_id, username: "joe"}
          member = %{user: user}
          assert UserCache.create(user)
          assert created_member = @cache.create(guild_id, member)

          assert [{^guild_id, ^created_member}] =
                   MemberCache.fold_by_user([], user_id, &collector/2, @cache)
        end
      end
    end
  end
end
