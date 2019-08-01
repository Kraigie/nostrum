defmodule Nostrum.Cache.Mapping.GuildShard do
  @moduledoc """
  Mapping of guild -> shard relationships.

  Guild servers do not inherently know which shard they came from. This mapping
  is an ETS table that stores this information.
  """

  alias Nostrum.Struct.Guild

  @doc """
  Gets the a shard num from a `guild_id`.
  """
  @spec get_shard(Guild.id()) :: {:ok, integer} | {:error, :id_not_found}
  def get_shard(guild_id) do
    case :ets.lookup(:guild_shard_map, guild_id) do
      [{_guild_id, shard_num}] -> {:ok, shard_num}
      [] -> {:error, :id_not_found}
    end
  end
end
