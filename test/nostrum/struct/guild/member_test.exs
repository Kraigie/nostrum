defmodule Nostrum.Struct.MemberTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Permission
  alias Nostrum.Struct.User

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
      guild = %Guild{roles: [%Role{id: 10, permissions: Permission.to_bitset(:administrator)}]}

      result = Member.guild_permissions(member, guild)

      assert(result === Permission.all())
    end

    test "returns all perms if owner" do
      member = %Member{user: %User{id: 200}}
      guild = %Guild{owner_id: 200}

      result = Member.guild_permissions(member, guild)

      assert(result === Permission.all())
    end
  end

  describe "String.Chars" do
    test "matches `mention/1`" do
      member = %Member{user: %User{id: 150_061_853_001_777_154}}

      assert(to_string(member) === Member.mention(member))
    end
  end
end
