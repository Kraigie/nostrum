defmodule Nostrum.Struct.Guild.RoleTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Guild.Role

  doctest Role

  describe "String.Chars" do
    test "matches `mention/1`" do
      role = %Role{id: 431_884_023_535_632_398}

      assert(to_string(role) === Role.mention(role))
    end
  end

  describe "Role.to_struct\1" do
    setup do
      etf_role = %{
        "id" => "431884023535632398",
        "name" => "cool people",
        "color" => 0x00FEED,
        "hoist" => true,
        "position" => 1,
        "permissions" => "8559918328",
        "managed" => false,
        "mentionable" => true,
        "icon" => "abc123",
        "unicode_emoji" => "🧪"
      }

      role = Role.to_struct(etf_role)

      {:ok, %{etf_role: etf_role, role: role}}
    end

    test "decodes to t:Role.t/0", context do
      assert(%Role{} = context.role)
    end

    test "decodes string permissions correctly", context do
      expected = String.to_integer(context.etf_role["permissions"])

      assert(is_integer(context.role.permissions))
      assert(expected === context.role.permissions)
    end
  end
end
