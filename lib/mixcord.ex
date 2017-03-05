defmodule Nostrum do
  @moduledoc false

  use Application

  @doc false
  def start(_, _) do
    import Supervisor.Spec

    token = Application.get_env(:nostrum, :token)
    num_shards = Application.get_env(:nostrum, :num_shards)

    if !token, do: raise "Please supply a token"
    actual_num_shards = if num_shards, do: num_shards, else: 1

    setup_ets_tables()

    children = [
      worker(Nostrum.Api.Ratelimiter, []),
      worker(Nostrum.Shard.Connector, []),
      supervisor(Nostrum.Cache.CacheSupervisor, []),
      supervisor(Nostrum.Shard.ShardSupervisor, [token, actual_num_shards])
    ]

    if Application.get_env(:nostrum, :dev, nil) do
      Supervisor.start_link(children ++ [supervisor(Dummy, [])], strategy: :one_for_one)
    else
      Supervisor.start_link(children, strategy: :one_for_one)
    end
  end

  @doc false
  def setup_ets_tables do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    :ets.new(:gateway_url, [:set, :public, :named_table])
    :ets.new(:unavailable_guilds, [:set, :public, :named_table])
    :ets.new(:users, [:set, :public, :named_table])
    :ets.new(:guild_shard_map, [:set, :public, :named_table])
    :ets.new(:channel_guild_map, [:set, :public, :named_table])
    :ets.new(:shard_pid_num, [:set, :public, :named_table])
  end

end
