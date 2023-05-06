defmodule Nostrum.Cache.MemberCacheTest do
  alias Nostrum.Cache.MemberCache
  alias Nostrum.Cache.UserCache
  alias Nostrum.Struct.Guild.Member
  use ExUnit.Case

  @moduledoc since: "0.7.0"

  @cache_modules [
    # Implementations
    Nostrum.Cache.MemberCache.ETS
  ]

  describe "get_with_users/1" do
    setup do
      [member_pid: start_supervised!(MemberCache), user_pid: start_supervised!(UserCache)]
    end

    test "with unknown guild" do
      assert [] = Enum.to_list(MemberCache.get_with_users(1_298_414))
    end

    test "guild with users" do
      guild_id = 123_890_195
      user_id = 128_309_125
      user = %{id: user_id, username: "joe"}
      member = %{user: user}
      assert created_user = UserCache.create(user)
      assert created_member = MemberCache.create(guild_id, member)

      stream = MemberCache.get_with_users(guild_id)
      assert [{^created_member, ^created_user}] = Enum.to_list(stream)
    end
  end

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

      doctest @cache

      setup do
        [pid: start_supervised!(@cache)]
      end

      test "member management" do
        # create/2
        # the gateway sends this differently, let's change it
        {user_id, raw} = Map.pop(@test_member, :user_id)
        as_payload = Map.put(raw, :user, %{id: user_id})
        expected = Member.to_struct(as_payload)
        member = @cache.create(@test_guild_id, as_payload)
        assert ^expected = member

        assert {:ok, ^member} = @cache.get(@test_guild_id, @test_member.user_id)

        # update/2
        payload = %{as_payload | nickname: "GrumblerBot3"}
        updated = Member.to_struct(payload)
        {guild_id, ^expected, ^updated} = @cache.update(@test_guild_id, payload)
        assert guild_id == @test_guild_id

        assert {:ok, ^member} = @cache.get(@test_guild_id, @test_member.user_id)

        # delete/2
        remove_payload = %{id: @test_member.user_id}
        {^guild_id, ^updated} = @cache.delete(@test_guild_id, remove_payload)
        guild_members = @cache.get(@test_guild_id)
        assert Enum.empty?(guild_members)
      end

      test "bulk_create/2" do
        # bulk_create receives the raw gateway-provided
        # member objects, which include the user.
        first_member = Map.put(@test_member, :user, %{id: @test_member.user_id})
        second_member = Map.put(%{@test_member | user_id: 12058}, :user, %{id: 12058})
        chunk = [first_member, second_member]
        casted_first_member = Member.to_struct(first_member)
        casted_second_member = Member.to_struct(second_member)
        assert true = @cache.bulk_create(@test_guild_id, chunk)

        members = Enum.sort(@cache.get(@test_guild_id), &(&1.user_id >= &2.user_id))
        assert [^casted_first_member, ^casted_second_member] = members
      end
    end
  end
end
