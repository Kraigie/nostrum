defmodule Nostrum.Store.UnavailableGuildMetaTest do
  alias Nostrum.Store.UnavailableGuild
  use ExUnit.Case, async: true

  @store_modules [
    # Dispatcher
    UnavailableGuild,
    # Implementations
    UnavailableGuild.ETS,
    UnavailableGuild.Mnesia
  ]

  for store <- @store_modules do
    defmodule :"#{store}Test" do
      use ExUnit.Case
      @store store
      doctest @store

      setup do
        Nostrum.Cache.TestBase.setup_and_teardown_cache(@store)
      end

      test "create/1 and is?/1" do
        guild_id = :erlang.unique_integer([:positive])
        refute @store.is?(guild_id)
        @store.create(guild_id)
        assert @store.is?(guild_id)
      end
    end
  end
end
