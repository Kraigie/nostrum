defmodule Nostrum.Struct.Guild.ChannelTest do
  use ExUnit.Case

  alias Nostrum.Struct.Guild.Channel

  doctest Channel

  describe "String.Chars" do
    test "matches `mention/1`" do
      channel = %Nostrum.Struct.Guild.Channel{id: 381889573426429952}

      assert(to_string(channel) === Channel.mention(channel))
    end
  end
end
