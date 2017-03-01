defmodule Mixcord.Cache.Mapping.GuildShard do
  @moduledoc """
  Mapping of guild -> shard relationships.

  Guild servers do not inherently know which shard they came from. This mapping
  is an ETS table that stores this information.
  """

  def get_shard(guild_id) do
    :ets.lookup_element(:guild_shard_map, guild_id, 2)
  end
  
end
