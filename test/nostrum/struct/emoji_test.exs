defmodule Nostrum.Struct.EmojiTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Emoji

  doctest Emoji

  describe "String.Chars" do
    test "matches `mention/1`" do
      emoji = %Emoji{id: 436885297037312001, name: "tealixir"}

      assert(to_string(emoji) === Emoji.mention(emoji))
    end
  end
end
