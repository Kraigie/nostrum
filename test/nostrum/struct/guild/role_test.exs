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
end
