defmodule Nostrum.Application do
  @moduledoc false

  use Application

  @doc false
  def start(_type, _args) do
    check_token()
    setup_ets_tables()

    children = [
      Nostrum.Api.Ratelimiter,
      Nostrum.Shard.Connector,
      Nostrum.Cache.CacheSupervisor,
      Nostrum.Shard.Supervisor
    ]

    if Application.get_env(:nostrum, :dev),
      do: Supervisor.start_link(children ++ [DummySupervisor], strategy: :one_for_one),
      else: Supervisor.start_link(children, strategy: :one_for_one)
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

  defp check_token, do: check_token(Application.get_env(:nostrum, :token))
  defp check_token(nil), do: raise("Please supply a token")
  defp check_token(<<_::192, 46, _::48, 46, _::216>>), do: :ok

  defp check_token(_invalid_format),
    do: raise("Invalid token format, copy it again from the `Bot` tab of your Application")
end
