defmodule Nostrum.Cache.GuildCacheTest do
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  use ExUnit.Case

  @cache_modules [
    # Implementations
    Nostrum.Cache.GuildCache.ETS
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

      describe "with an empty cache" do
        setup do
          [pid: start_supervised!(@cache)]
        end

        test "all/0 returns empty enum" do
          assert Enum.to_list(@cache.all) == []
        end

        test "create/1 returns true" do
          assert @cache.create(@test_guild) == true
        end
      end

      describe "with cached guild" do
        setup do
          pid = start_supervised!(@cache)
          guild = Guild.to_struct(@test_guild)
          true = @cache.create(guild)
          [pid: pid]
        end

        test "all/0 returns guild" do
          all_list = Enum.to_list(@cache.all())
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
        setup do
          pid = start_supervised!(@cache)
          guild = Guild.to_struct(@test_guild)
          true = @cache.create(guild)
          [pid: pid]
        end

        test "update and delete" do
          # update/1
          new_guild = %{@test_guild | name: "Merkel's Merkle Trees"}
          old = Guild.to_struct(@test_guild)
          new = Guild.to_struct(new_guild)
          assert {^old, ^new} = @cache.update(new_guild)

          # delete/1
          assert ^new = @cache.delete(@test_guild.id)
          assert @cache.delete(@test_guild.id) == nil
        end

        test "member count operations" do
          assert {:ok, %{member_count: 0}} = @cache.get(@test_guild.id)
          assert @cache.member_count_up(@test_guild.id)
          assert {:ok, %{member_count: 1}} = @cache.get(@test_guild.id)
          assert @cache.member_count_down(@test_guild.id)
          assert {:ok, %{member_count: 0}} = @cache.get(@test_guild.id)
        end
      end
    end
  end
end
