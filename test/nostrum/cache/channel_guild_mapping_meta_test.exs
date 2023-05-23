defmodule Nostrum.Cache.ChannelGuildMappingMetaTest do
  alias Nostrum.Cache.ChannelGuildMapping
  use ExUnit.Case, async: true

  @cache_modules [
    # Dispatcher
    ChannelGuildMapping,
    # Implementations
    ChannelGuildMapping.ETS,
    ChannelGuildMapping.Mnesia
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      use ExUnit.Case
      @cache cache
      doctest @cache

      setup do
        if function_exported?(@cache, :teardown, 0) do
          on_exit(:cleanup, fn -> apply(@cache, :teardown, []) end)
        end

        [pid: start_supervised!(@cache)]
      end

      test "storing functionality" do
        channel_id = :erlang.unique_integer([:positive])
        guild_id = :erlang.unique_integer([:positive])

        refute @cache.get(channel_id)
        assert @cache.delete(channel_id)

        assert @cache.create(channel_id, guild_id)
        assert ^guild_id = @cache.get(channel_id)

        assert @cache.delete(channel_id)
        refute @cache.get(channel_id)
      end
    end
  end
end
