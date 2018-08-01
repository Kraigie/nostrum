defmodule Nostrum.Struct.MemberTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.{Channel, Guild, Permission, User, Overwrite}
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
      guild = %Guild{roles: %{role.id => role}}

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
        roles: %{role.id => role}
      }

      result = Member.guild_permissions(member, guild)

      assert(result === Permission.from_bitset!(0x00000021))
    end
  end

  describe "guild_channel_permissions/3" do
    test "returns all perms if member is admin" do
      member_id = 500
      role_id = 200
      channel_id = 100

      member = %Member{user: %User{id: member_id}, roles: [role_id]}
      role = %Role{id: role_id, permissions: 0x00000008}
      channel = %Channel{id: channel_id}

      guild = %Guild{
        channels: %{channel.id => channel},
        members: %{member.user.id => member},
        roles: %{role.id => role}
      }

      result = Member.guild_channel_permissions(member, guild, channel_id)

      assert(result === Permission.all())
    end

    test "role overwrites have priority over @everyone overwrites" do
      member_id = 500
      guild_id = 100
      role_id = 200
      channel_id = 300
      everyone_role_id = 100

      test_perm_bits = 0x00000040

      member = %Member{user: %User{id: member_id}, roles: [everyone_role_id, role_id]}

      everyone_role = %Role{id: everyone_role_id, permissions: 0}
      role = %Role{id: role_id, permissions: 0}

      channel = %Channel{
        id: channel_id,
        permission_overwrites: [
          %Overwrite{id: role_id, allow: test_perm_bits, deny: 0},
          %Overwrite{id: everyone_role_id, deny: test_perm_bits, allow: 0}
        ]
      }

      guild = %Guild{
        id: guild_id,
        channels: %{channel.id => channel},
        roles: %{everyone_role.id => everyone_role, role.id => role}
      }

      result = Member.guild_channel_permissions(member, guild, channel_id)

      assert(result === Permission.from_bitset!(0x00000040))
    end

    test "member overwrites have priority over role overwrites" do
      member_id = 500
      guild_id = 100
      role_id = 200
      channel_id = 300
      everyone_role_id = 100

      test_perm_bits = 0x00000040

      member = %Member{user: %User{id: member_id}, roles: [role_id]}

      everyone_role = %Role{id: everyone_role_id, permissions: 0}
      role = %Role{id: role_id, permissions: 0}

      channel = %Channel{
        id: channel_id,
        permission_overwrites: [
          %Overwrite{id: role_id, allow: 0, deny: test_perm_bits},
          %Overwrite{id: member_id, allow: test_perm_bits, deny: 0}
        ]
      }

      guild = %Guild{
        id: guild_id,
        channels: %{channel.id => channel},
        roles: %{everyone_role.id => everyone_role, role.id => role}
      }

      result = Member.guild_channel_permissions(member, guild, channel_id)

      assert(result === Permission.from_bitset!(0x00000040))
    end
  end

  describe "String.Chars" do
    test "matches `mention/1`" do
      member = %Member{user: %User{id: 150_061_853_001_777_154}}

      assert(to_string(member) === Member.mention(member))
    end
  end
end
