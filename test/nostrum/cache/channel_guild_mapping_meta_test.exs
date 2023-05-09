defmodule Nostrum.Cache.ChannelGuildMappingMetaTest do
  alias Nostrum.Cache.ChannelGuildMapping
  use ExUnit.Case

  @cache_modules [
    # Dispatcher
    ChannelGuildMapping,
    # Implementations
    ChannelGuildMapping.ETS
  ]

  for cache <- @cache_modules do
    defmodule :"#{cache}Test" do
      use ExUnit.Case
      @cache cache
      doctest @cache

      setup do
        [pid: start_supervised!(@cache)]
      end

      test "storing functionality" do
        channel_id = :erlang.unique_integer([:positive])
        guild_id = :erlang.unique_integer([:positive])

        refute ChannelGuildMapping.get(channel_id)
        assert ChannelGuildMapping.delete(channel_id)

        assert ChannelGuildMapping.create(channel_id, guild_id)
        assert ^guild_id = ChannelGuildMapping.get(channel_id)

        assert ChannelGuildMapping.delete(channel_id)
        refute ChannelGuildMapping.get(channel_id)
      end
    end
  end
end
