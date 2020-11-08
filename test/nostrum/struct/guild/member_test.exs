defmodule Nostrum.Struct.MemberTest do
  use ExUnit.Case, async: true

  alias Nostrum.Permission
  alias Nostrum.Struct.{Channel, Guild, User, Overwrite}
  alias Nostrum.Struct.Guild.{Member, Role}

  doctest Member

  describe "mention/1" do
    test "matches `Nostrum.Struct.User.mention/1`" do
      member = %Member{user: %User{id: 150_061_853_001_777_154}}

      assert(Member.mention(member) === User.mention(member.user))
    end
  end

  describe "guild_permissions/2" do
    test "returns all perms if admin" do
      member = %Member{roles: [10]}
      role = %Role{id: 10, permissions: Permission.to_bitset([:administrator])}
      guild = %Guild{roles: [role]}

      result = Member.guild_permissions(member, guild)

      assert(result === Permission.all())
    end

    test "returns all perms if owner" do
      member = %Member{user: %User{id: 200}}
      guild = %Guild{owner_id: 200}

      result = Member.guild_permissions(member, guild)

      assert(result === Permission.all())
    end

    test "returns permissions otherwise" do
      member = %Member{roles: [10]}
      role_perms = [:create_instant_invite, :manage_guild]
      role = %Role{id: 10, permissions: Permission.to_bitset(role_perms)}

      guild = %Guild{
        roles: [role]
      }

      result = Member.guild_permissions(member, guild)

      assert(result === Permission.from_bitset(0x00000021))
    end
  end

  describe "guild_channel_permissions/3" do
    setup do
      [
        channel_id: 300,
        everyone_role_id: 100,
        guild_id: 100,
        member_id: 500,
        role_id: 200
      ]
    end

    test "returns all perms if member is admin", context do
      member = %Member{user: %User{id: context[:member_id]}, roles: [context[:role_id]]}
      role = %Role{id: context[:role_id], permissions: 0x00000008}
      channel = %Channel{id: context[:channel_id]}

      guild = %Guild{
        channels: [channel],
        members: [member],
        roles: [role]
      }

      result = Member.guild_channel_permissions(member, guild, context[:channel_id])

      assert(result === Permission.all())
    end

    test "role overwrites have priority over @everyone overwrites", context do
      test_perm_bits = 0x00000040

      member = %Member{
        user: %User{id: context[:member_id]},
        roles: [context[:everyone_role_id], context[:role_id]]
      }

      everyone_role = %Role{id: context[:everyone_role_id], permissions: 0}
      role = %Role{id: context[:role_id], permissions: 0}

      channel = %Channel{
        id: context[:channel_id],
        permission_overwrites: [
          %Overwrite{id: context[:role_id], allow: test_perm_bits, deny: 0},
          %Overwrite{id: context[:everyone_role_id], deny: test_perm_bits, allow: 0}
        ]
      }

      guild = %Guild{
        id: context[:guild_id],
        channels: [channel],
        roles: [everyone_role, role]
      }

      result = Member.guild_channel_permissions(member, guild, context[:channel_id])

      assert(result === Permission.from_bitset(0x00000040))
    end

    test "member overwrites have priority over role overwrites", context do
      test_perm_bits = 0x00000040

      member = %Member{user: %User{id: context[:member_id]}, roles: [context[:role_id]]}

      everyone_role = %Role{id: context[:everyone_role_id], permissions: 0}
      role = %Role{id: context[:role_id], permissions: 0}

      channel = %Channel{
        id: context[:channel_id],
        permission_overwrites: [
          %Overwrite{id: context[:role_id], allow: 0, deny: test_perm_bits},
          %Overwrite{id: context[:member_id], allow: test_perm_bits, deny: 0}
        ]
      }

      guild = %Guild{
        id: context[:guild_id],
        channels: [channel],
        roles: [everyone_role, role]
      }

      result = Member.guild_channel_permissions(member, guild, context[:channel_id])

      assert(result === Permission.from_bitset(0x00000040))
    end

    test "returns empty list when there are no matching ids between channel overrides and member roles",
         context do
      member = %Member{user: %User{id: context[:member_id]}, roles: [context[:role_id]]}

      everyone_role = %Role{id: context[:everyone_role_id], permissions: 0}
      role = %Role{id: context[:role_id], permissions: 0}

      channel = %Channel{
        id: context[:channel_id],
        permission_overwrites: [
          %Overwrite{id: context[:role_id], allow: 0, deny: 0},
          %Overwrite{id: context[:member_id], allow: 0, deny: 0}
        ]
      }

      guild = %Guild{
        id: context[:guild_id],
        channels: [channel],
        roles: [everyone_role, role]
      }

      result = Member.guild_channel_permissions(member, guild, context[:channel_id])

      assert(result == [])
    end
  end

  describe "String.Chars" do
    test "matches `mention/1`" do
      member = %Member{user: %User{id: 150_061_853_001_777_154}}

      assert(to_string(member) === Member.mention(member))
    end
  end
end
