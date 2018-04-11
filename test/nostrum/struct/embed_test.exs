defmodule Nostrum.Struct.EmbedTest do
  use ExUnit.Case

  alias Nostrum.Struct.Embed
  alias Nostrum.Struct.Embed.Field

  require Nostrum.Struct.Embed

  describe "embed builder functions" do
    test "if given embed info, then they modify the embed correctly" do
      import Nostrum.Struct.Embed

      expected = %Embed{
        title: "craig",
        description: "nostrum",
        url: "https://google.com/",
        timestamp: "2016-05-05T21:04:13.203Z",
        color: 431_948,
        fields: [
          %Field{name: "Field 1", value: "Test"},
          %Field{name: "Field 2", value: "More test", inline: true}
        ]
      }

      observed =
        %Embed{}
        |> put_title("craig")
        |> put_description("nostrum")
        |> put_url("https://google.com/")
        |> put_timestamp("2016-05-05T21:04:13.203Z")
        |> put_color(431_948)
        |> put_field("Field 1", "Test")
        |> put_field("Field 2", "More test", true)

      assert expected === observed
    end
  end
end
