defmodule Nostrum.Cache.Mapping.ChannelGuild do
  @moduledoc """
  Mapping of channel -> guild relationships.

  Given just the `id` of a guild channel, there's no way to know what guild it
  belongs to. This mapping is an ETS table that stores this information.
  """

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild

  @doc """
  Gets a `guild_id` from a `channel_id`.
  """
  @spec get_guild(Channel.id()) :: {:ok, Guild.id()} | {:error, :id_not_found}
  def get_guild(channel_id) do
    case :ets.lookup(:channel_guild_map, channel_id) do
      [{_channel_id, guild_id}] -> {:ok, guild_id}
      [] -> {:error, :id_not_found}
    end
  end
end
