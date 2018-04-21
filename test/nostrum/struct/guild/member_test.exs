defmodule Nostrum.Struct.MemberTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User

  doctest Member

  describe "mention/1" do
    test "matches `Nostrum.Struct.User.mention/1`" do
      member = %Member{user: %User{id: 150061853001777154}}

      assert(Member.mention(member) === User.mention(member.user))
    end
  end

  describe "String.Chars" do
    test "matches `mention/1`" do
      member = %Member{user: %User{id: 150061853001777154}}

      assert(to_string(member) === Member.mention(member))
    end
  end
end
