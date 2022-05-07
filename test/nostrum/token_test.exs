defmodule Nostrum.TokenTest do
  use ExUnit.Case, async: true

  alias Nostrum.Token

  doctest Token

  describe "check_token!/1" do
    test "raises on non set token" do
      assert_raise RuntimeError, "A bot token needs to be supplied in your config file", fn ->
        Token.check_token!(nil)
      end
    end

    test "raises on wrong token format" do
      assert_raise RuntimeError,
                   ~S[Invalid token format. Copy it again from the "Bot" tab of your Application in the Discord Developer Portal.],
                   fn ->
                     Token.check_token!("666")
                   end
    end
  end
end
