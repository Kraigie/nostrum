defmodule Nostrum.Store.UnavailableGuildMetaTest do
  alias Nostrum.Store.UnavailableGuild
  use ExUnit.Case

  @store_modules [
    # Dispatcher
    UnavailableGuild,
    # Implementations
    UnavailableGuild.ETS
  ]

  for store <- @store_modules do
    defmodule :"#{store}Test" do
      use ExUnit.Case
      @store store
      doctest @store

      setup do
        [pid: start_supervised!(@store)]
      end

      test "create/1 and is?/1" do
        guild_id = :erlang.unique_integer([:positive])
        refute UnavailableGuild.is?(guild_id)
        UnavailableGuild.create(guild_id)
        assert UnavailableGuild.is?(guild_id)
      end
    end
  end
end
