defmodule Nostrum.Store.GuildShardMappingMetaTest do
  alias Nostrum.Store.GuildShardMapping
  use ExUnit.Case

  @store_modules [
    # Dispatcher
    GuildShardMapping,
    # Implementations
    GuildShardMapping.ETS
  ]

  for store <- @store_modules do
    defmodule :"#{store}Test" do
      use ExUnit.Case
      @store store
      doctest @store

      setup do
        [pid: start_supervised!(@store)]
      end

      test "storing functionality" do
        guild_id = :erlang.unique_integer([:positive])
        shard_num = :erlang.unique_integer([:positive])

        refute GuildShardMapping.get(guild_id)
        assert GuildShardMapping.delete(guild_id)

        assert GuildShardMapping.create(guild_id, shard_num)
        assert ^shard_num = GuildShardMapping.get(guild_id)

        assert GuildShardMapping.delete(guild_id)
        refute GuildShardMapping.get(guild_id)
      end
    end
  end
end
