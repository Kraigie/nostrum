defmodule Nostrum do
  @moduledoc false

  use Application

  @doc false
  def start(_, _) do
    import Supervisor.Spec

    if !Application.get_env(:nostrum, :token), do: raise("Please supply a token")

    num_shards = Application.get_env(:nostrum, :num_shards)
    corrected_num_shards = if num_shards, do: num_shards, else: 1

    setup_ets_tables()

    children = [
      Nostrum.Api.Ratelimiter,
      Nostrum.Shard.Connector,
      Nostrum.Cache.CacheSupervisor,
      {Nostrum.Shard.Supervisor, corrected_num_shards}
    ]

    if Application.get_env(:nostrum, :dev, nil) do
      Supervisor.start_link(children ++ [supervisor(Dummy, [])], strategy: :one_for_one)
    else
      Supervisor.start_link(children, strategy: :one_for_one)
    end
  end

  @doc false
  def setup_ets_tables do
    :ets.new(:gateway_url, [:set, :public, :named_table])
    :ets.new(:unavailable_guilds, [:set, :public, :named_table])
    :ets.new(:users, [:set, :public, :named_table])
    :ets.new(:channels, [:set, :public, :named_table])
    :ets.new(:presences, [:set, :public, :named_table])
    :ets.new(:guild_shard_map, [:set, :public, :named_table])
    :ets.new(:channel_guild_map, [:set, :public, :named_table])
  end
end
