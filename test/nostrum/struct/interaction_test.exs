defmodule Nostrum.Struct.InteractionTest do
  alias Nostrum.Struct.{
    ApplicationCommandInteractionData,
    ApplicationCommandInteractionDataOption,
    ApplicationCommandInteractionDataResolved,
    Interaction,
    User
  }

  alias Nostrum.Struct.Guild.{Role, Member}
  alias Nostrum.Util

  use ExUnit.Case, async: true

  @payload %{
    "application_id" => 455_589_479_713_865_749,
    "channel_id" => 455_589_923_626_549_259,
    "data" => %{
      "id" => 793_152_718_839_087_135,
      "name" => "role",
      "options" => [
        %{"name" => "name", "type" => 8, "value" => "451824027976073216"},
        %{"name" => "action", "type" => 3, "value" => "assign"}
      ],
      "resolved" => %{
        "roles" => %{
          "451824027976073216" => %{
            "color" => 0,
            "hoist" => false,
            "id" => 451_824_027_976_073_216,
            "managed" => false,
            "mentionable" => false,
            "name" => "@everyone",
            "permissions" => "109625859648",
            "position" => 0
          }
        }
      }
    },
    "guild_id" => 451_824_027_976_073_216,
    "id" => 857_721_405_302_112_276,
    "member" => %{
      "avatar" => nil,
      "deaf" => false,
      "is_pending" => false,
      "joined_at" => "2018-05-31T19:07:22.755000+00:00",
      "mute" => false,
      "nick" => "blob",
      "pending" => false,
      "permissions" => "137438953471",
      "premium_since" => nil,
      "roles" => [451_824_750_399_062_036, 458_692_275_199_803_406],
      "user" => %{
        "avatar" => "deadbeef",
        "discriminator" => "1234",
        "id" => 149_120,
        "public_flags" => 0,
        "username" => "Joe Armstrong"
      }
    },
    "token" => "bob",
    "type" => 2,
    "version" => 1
  }

  describe "Interaction.to_struct/1" do
    test "casts interaction properly" do
      atom_mapped = Util.safe_atom_map(@payload)

      assert %Interaction{
               application_id: 455_589_479_713_865_749,
               channel_id: 455_589_923_626_549_259,
               data: %ApplicationCommandInteractionData{
                 id: 793_152_718_839_087_135,
                 name: "role",
                 options: [
                   %ApplicationCommandInteractionDataOption{
                     name: "name",
                     type: 8,
                     value: 451_824_027_976_073_216
                   },
                   %ApplicationCommandInteractionDataOption{
                     name: "action",
                     type: 3,
                     value: "assign"
                   }
                 ],
                 resolved: %ApplicationCommandInteractionDataResolved{
                   roles: %{
                     451_824_027_976_073_216 => %Role{}
                   }
                 }
               },
               guild_id: 451_824_027_976_073_216,
               id: 857_721_405_302_112_276,
               member: %Member{
                 user_id: 149_120
               },
               user: %User{},
               token: "bob",
               type: 2,
               version: 1
             } = Interaction.to_struct(atom_mapped)
    end
  end
end
