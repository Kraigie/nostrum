defmodule Nostrum.Application do
  @moduledoc false

  use Application

  @doc false
  def start(_, _) do
    import Supervisor.Spec

    num_shards = Application.get_env(:nostrum, :num_shards, 1)

    validate_token()
    setup_ets_tables()

    children = [
      Nostrum.Api.Ratelimiter,
      Nostrum.Shard.Connector,
      Nostrum.Cache.CacheSupervisor,
      {Nostrum.Shard.Supervisor, num_shards}
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

  defp validate_token() do
    token = Application.get_env(:nostrum, :token)
    unless token, do: raise("Please supply a token")
    unless token =~ ~r/[\w-]{24}\.[\w-]{6}\.[\w-]{27}/, do: raise(
      """
      Invalid token. You can find the token of your bot in the "bot" tab for your Application.
      """)
  end
end
