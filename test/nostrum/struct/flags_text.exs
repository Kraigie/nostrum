defmodule Nostrum.Struct.FlagsTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Flags

  doctest Flags

  describe "test flag serialise deserialise" do
    test "from_integer/1" do
      flags = Flags.from_integer(131_842)

      assert(flags.early_supporter == true)
      assert(flags.hypesquad_balance == true)
      assert(flags.partner == true)
      assert(flags.verified_developer == true)
      assert(flags.staff == false)
    end

    test "to_integer/1" do
      flags = %Flags{
        early_supporter: true,
        hypesquad_balance: true,
        partner: true,
        verified_developer: true
      }

      assert(Flags.to_integer(flags) == 131_842)
    end
  end
end
