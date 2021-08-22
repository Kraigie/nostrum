defmodule Nostrum.Cache.GuildCacheTest do
  use ExUnit.Case

  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Guild

  setup_all do
    :ets.new(GuildCache.tabname(), [:set, :public, :named_table])
    assert true = GuildCache.create(%Guild{id: 0})
    :ok
  end

  doctest GuildCache
end
