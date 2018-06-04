defmodule Nostrum.Struct.Invite.MetadataTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Invite.Metadata
  alias Nostrum.Struct.User
  alias Nostrum.Util

  doctest Metadata

  describe "Metadata.to_struct/1" do
    setup do
      etf_metadata = %{
        "inviter" => %{
          "id" => "80351110224678912",
          "username" => "Nelly",
          "discriminator" => "1337",
          "avatar" => "8342729096ea3675442027381ff50dfe",
          "verified" => true,
          "email" => "nelly@discordapp.com"
        },
        "uses" => 0,
        "max_uses" => 0,
        "max_age" => 0,
        "temporary" => false,
        "created_at" => "2016-03-31T19:15:39.954000+00:00",
        "revoked" => false
      }

      metadata = Metadata.to_struct(etf_metadata)

      {:ok, %{etf_metadata: etf_metadata, metadata: metadata}}
    end

    test "decodes to `t:Metadata.t/0`", context do
      assert(%Metadata{} = context.metadata)
    end

    test "decodes inviter correctly", context do
      expected = context.etf_metadata["inviter"] |> Util.cast({:struct, User})

      assert(expected === context.metadata.inviter)
    end
  end
end
