defmodule Nostrum.Cache.Mapping.GuildShard do
  @moduledoc """
  Mapping of guild -> shard relationships.

  Guild servers do not inherently know which shard they came from. This mapping
  is an ETS table that stores this information.
  """

  @doc """
  Gets the a shard num from a `guild_id`.
  """
  @spec get_shard(integer) :: integer
  def get_shard(guild_id) do
    :ets.lookup_element(:guild_shard_map, guild_id, 2)
  end

end
