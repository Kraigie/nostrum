defmodule Nostrum.Struct.UserTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.User

  doctest User

  describe "String.Chars" do
    test "matches `mention/1`" do
      user = %User{id: 150_061_853_001_777_154}

      assert(to_string(user) === User.mention(user))
    end
  end
end
