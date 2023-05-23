defmodule Nostrum.Store.GuildShardMappingMetaTest do
  alias Nostrum.Store.GuildShardMapping
  use ExUnit.Case, async: true

  @store_modules [
    # Dispatcher
    GuildShardMapping,
    # Implementations
    GuildShardMapping.ETS,
    GuildShardMapping.Mnesia
  ]

  for store <- @store_modules do
    defmodule :"#{store}Test" do
      use ExUnit.Case
      @store store
      doctest @store

      setup do
        on_exit(:cleanup, fn ->
          try do
            if function_exported?(@store, :teardown, 0) do
              apply(@store, :teardown, [])
            end
          rescue
            e -> e
          end
        end)

        [pid: start_supervised!(@store)]
      end

      test "storing functionality" do
        guild_id = :erlang.unique_integer([:positive])
        shard_num = :erlang.unique_integer([:positive])

        refute @store.get(guild_id)
        assert @store.delete(guild_id)

        assert @store.create(guild_id, shard_num)
        assert ^shard_num = @store.get(guild_id)

        assert @store.delete(guild_id)
        refute @store.get(guild_id)
      end
    end
  end
end
