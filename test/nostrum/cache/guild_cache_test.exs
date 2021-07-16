defmodule Nostrum.Cache.GuildCacheTest do
  use ExUnit.Case

  alias Nostrum.Cache.CacheSupervisor
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Guild

  setup_all do
    start_supervised!(CacheSupervisor, [])
    assert true = GuildCache.create(%Guild{id: 0})
    :ok
  end

  doctest GuildCache
end
