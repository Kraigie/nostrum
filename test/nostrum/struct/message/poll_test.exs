defmodule Nostrum.Struct.Message.PollTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Message.Poll

  doctest Poll

  test "Poll.create_poll/2" do
    assert Poll.create_poll("Craigs Cats!", duration: 1, allow_multiselect: true) ==
             %Nostrum.Struct.Message.Poll{
               question: %Nostrum.Struct.Message.Poll.MediaObject{
                 text: "Craigs Cats!",
                 emoji: nil
               },
               answers: [],
               expiry: nil,
               duration: 1,
               allow_multiselect: true,
               layout_type: 1,
               results: nil
             }
  end

  test "Poll.put_answer/2" do
    poll =
      Poll.create_poll("Craigs Cats!", duration: 1, allow_multiselect: true)
      |> Poll.put_answer("Yes!")

    assert Enum.at(poll.answers, 0).poll_media.text == "Yes!"
  end

  test "Poll.put_answer/3" do
    poll =
      Poll.create_poll("Craigs Cats!", duration: 1, allow_multiselect: true)
      |> Poll.put_answer("Yes!", custom_emoji: 112_233_445_566_778_899)

    assert Enum.at(poll.answers, 0).poll_media.text == "Yes!"
    assert Enum.at(poll.answers, 0).poll_media.emoji.id == 112_233_445_566_778_899
  end
end
