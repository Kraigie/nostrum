defmodule Nostrum.Struct.InviteTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Invite
  alias Nostrum.Struct.Invite.Metadata
  alias Nostrum.Struct.User
  alias Nostrum.Util

  doctest Invite

  describe "Invite.to_struct/1" do
    setup do
      external_invite = %{
        "code" => "0vCdhLbwjZZTWZLD",
        "guild" => %{
          "id" => "165176875973476352",
          "name" => "CS:GO Fraggers Only",
          "splash" => nil,
          "icon" => nil
        },
        "channel" => %{
          "id" => "165176875973476352",
          "name" => "illuminati",
          "type" => 0
        },
        "inviter" => {},
        "uses" => 0,
        "max_uses" => 0,
        "max_age" => 0,
        "temporary" => false,
        "created_at" => "2016-03-31T19:15:39.954000+00:00",
        "revoked" => false
      }

      invite = Invite.to_struct(external_invite)

      {:ok, %{external_invite: external_invite, invite: invite}}
    end

    test "decodes guild correctly", %{invite: invite} do
      expected = %Guild{
        id: 165_176_875_973_476_352,
        name: "CS:GO Fraggers Only",
        splash: nil,
        icon: nil
      }

      assert(^expected = invite.guild)
    end

    test "decodes channel correctly", %{invite: invite} do
      expected = %Channel{
        id: 165_176_875_973_476_352,
        name: "illuminati",
        type: 0
      }

      assert(^expected = invite.channel)
    end

    test "decodes metadata correctly", context do
      expected = %Metadata{
        inviter: %User{
          id: 80_351_110_224_678_912,
          username: "Nelly",
          discriminator: "1337",
          avatar: "8342729096ea3675442027381ff50dfe",
          verified: true,
          email: "nelly@discordapp.com"
        },
        uses: 0,
        max_uses: 0,
        max_age: 0,
        temporary: false,
        created_at: "2016-03-31T19:15:39.954000+00:00",
        revoked: false
      }

      assert(^expected = invite.metadata)
    end
  end
end
