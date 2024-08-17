defmodule Nostrum.Cache.GuildCacheMetaTest do
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  use ExUnit.Case, async: true

  @cache_modules [
    # Dispatcher
    GuildCache,
    # Implementations
    GuildCache.ETS,
    GuildCache.Mnesia
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      use ExUnit.Case
      # this is needed because otherwise we cannot access
      # the cache in the tests
      @cache cache
      @test_guild %{
        id: 1234,
        name: "joe's jobs",
        channels: %{},
        emojis: [],
        roles: %{},
        member_count: 0
      }
      @test_channel %{
        id: 91231,
        name: "Joe's Grumblings"
      }
      @test_role %{
        id: 102_512,
        name: "High Sharders"
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

      @doc """
      Collects all results from a `fold` query in the accumulator.
      """
      @doc since: "0.8.0"
      def collector(item, acc) do
        [item | acc]
      end

      describe "with an empty cache" do
        test "fold/3 returns empty enum" do
          assert Enum.to_list(GuildCache.fold([], &collector/2, @cache)) == []
        end

        test "create/1 returns guild" do
          expected = Guild.to_struct(@test_guild)
          assert ^expected = @cache.create(@test_guild)
        end

        test "update/1 returns {nil, guild}" do
          expected = Guild.to_struct(@test_guild)
          assert {nil, ^expected} = @cache.update(@test_guild)
        end

        test "delete/1 returns nil" do
          assert nil == @cache.delete(@test_guild.id)
        end
      end

      describe "with cached guild" do
        setup context do
          guild = Guild.to_struct(@test_guild)
          ^guild = @cache.create(@test_guild)
          context
        end

        test "all/0 returns guild" do
          all_list = Enum.to_list(GuildCache.fold([], &collector/2, @cache))
          expected = Guild.to_struct(@test_guild)
          assert [^expected] = all_list
        end

        test "channel management" do
          # channel_create/1
          created = @cache.channel_create(@test_guild.id, @test_channel)
          expected = Channel.to_struct(@test_channel)
          assert ^expected = created
          {:ok, cached} = @cache.get(@test_guild.id)
          channels = %{expected.id => expected}
          assert ^channels = cached.channels

          # channel_update/1
          updated_channel = Map.put(@test_channel, :name, "Craig's Grumbling")
          expected = Channel.to_struct(updated_channel)
          {old, new} = @cache.channel_update(@test_guild.id, updated_channel)
          old_name = @test_channel.name
          assert %Channel{name: ^old_name} = old
          assert ^expected = new

          # channel_delete/1
          deleted = @cache.channel_delete(@test_guild.id, @test_channel.id)
          assert ^expected = deleted

          # Double delete!
          assert :noop = @cache.channel_delete(@test_guild.id, @test_channel.id)
        end

        test "emoji_update/1" do
          payload_emoji = %{id: 102_394, name: "joeface3"}
          emojis = [payload_emoji]
          expected = [Emoji.to_struct(payload_emoji)]
          {old, new} = @cache.emoji_update(@test_guild.id, emojis)
          assert [] = old
          assert ^expected = new
        end

        test "role management" do
          # role_create/2
          expected_guild_id = @test_guild.id
          expected_role_struct = Role.to_struct(@test_role)
          expected_return = {expected_guild_id, expected_role_struct}
          role_id = expected_role_struct.id
          assert ^expected_return = @cache.role_create(@test_guild.id, @test_role)

          assert {:ok, %Guild{roles: %{^role_id => ^expected_role_struct}}} =
                   @cache.get(@test_guild.id)

          # role_update/2
          updated_payload = Map.put(@test_role, :name, "Higher Sharders")
          new_role = Role.to_struct(updated_payload)

          {^expected_guild_id, ^expected_role_struct, ^new_role} =
            @cache.role_update(@test_guild.id, updated_payload)

          # role_delete/2
          {_guild_id, ^new_role} = @cache.role_delete(@test_guild.id, @test_role.id)
          {:ok, guild} = @cache.get(@test_guild.id)
          assert guild.roles == %{}
        end
      end

      describe "guild" do
        setup context do
          guild = Guild.to_struct(@test_guild)
          ^guild = @cache.create(@test_guild)
          context
        end

        test "member count operations" do
          assert {:ok, %{member_count: 0}} = @cache.get(@test_guild.id)
          assert @cache.member_count_up(@test_guild.id)
          assert {:ok, %{member_count: 1}} = @cache.get(@test_guild.id)
          assert @cache.member_count_down(@test_guild.id)
          assert {:ok, %{member_count: 0}} = @cache.get(@test_guild.id)
        end

        test "member count operations for uncached guild" do
          assert @cache.member_count_up(@test_guild.id + 1)
          assert @cache.member_count_down(@test_guild.id + 1)
        end

        test "update and delete" do
          # update/1
          new_guild = %{@test_guild | name: "Merkel's Merkle Trees"}
          old = Guild.to_struct(@test_guild)
          new = Guild.to_struct(new_guild)
          assert {^old, ^new} = @cache.update(new_guild)

          # delete/1
          assert ^new = @cache.delete(@test_guild.id)
          refute @cache.delete(@test_guild.id)
        end
      end
    end
  end
end
