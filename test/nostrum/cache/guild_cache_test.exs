defmodule Nostrum.Cache.GuildCacheTest do
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
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
        members: %{},
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
      @test_member %{
        roles: [],
        user: %{
          id: 120_391,
          name: "joe"
        }
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

        test "member management" do
          # member_add/2
          member_id = @test_member.user.id
          expected = Member.to_struct(@test_member)
          member = @cache.member_add(@test_guild.id, @test_member)
          assert ^expected = member

          assert {:ok, %{^member_id => ^expected}} =
                   @cache.select_by([id: @test_guild.id], & &1.members)

          # member_update/2
          payload = put_in(@test_member, [:user, :name], "GrumblerBot3")
          updated = Member.to_struct(payload)
          {guild_id, ^expected, ^updated} = @cache.member_update(@test_guild.id, payload)
          assert guild_id == @test_guild.id

          assert {:ok, %{^member_id => ^updated}} =
                   @cache.select_by([id: @test_guild.id], & &1.members)

          # member_remove/2
          remove_payload = payload.user
          {^guild_id, ^updated} = @cache.member_remove(@test_guild.id, remove_payload)
          {:ok, guild_members} = @cache.select_by([id: @test_guild.id], & &1.members)
          assert Enum.empty?(guild_members)
        end

        # Copying the comment from the ETS cache implementation:
        #   We may retrieve a GUILD_MEMBER_UPDATE event for our own user even if we
        #   have the required intents to retrieve it for other members disabled, as
        #   outlined in issue https://github.com/Kraigie/nostrum/issues/293. In
        #   that case, we will not have the guild cached.
        # This test verifies that the cache guards against that.
        test "member_update/2 handles uncached guild" do
          guild = Map.put(@test_guild, :id, @test_guild.id + 12839)
          expected = Member.to_struct(@test_member)
          {_guild_id, nil, ^expected} = @cache.member_update(guild, @test_member)
        end

        test "member_chunk/2" do
          second_member = put_in(@test_member, [:user, :id], 19204)
          chunk = [@test_member, second_member]
          assert true = @cache.member_chunk(@test_guild.id, chunk)
          first_id = @test_member.user.id
          second_id = second_member.user.id

          assert {:ok, %Guild{members: %{^first_id => _a, ^second_id => _b}}} =
                   @cache.get(@test_guild.id)
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
      end
    end
  end
end
