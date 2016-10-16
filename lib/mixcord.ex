defmodule Mixcord do
  @moduledoc """
  """

  use Application

  def start(_, _) do
    import Supervisor.Spec

    token = Application.get_env(:mixcord, :token)
    caller = Application.get_env(:mixcord, :caller)
    num_shards = Application.get_env(:mixcord, :num_shards)

    setup_ets_tables

    children = [
      supervisor(Mixcord.Cache.Supervisor, []),
      supervisor(Mixcord.Shard.Supervisor, [token, caller, num_shards])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def setup_ets_tables do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    :ets.new(:gateway_url, [:set, :public, :named_table])
  end

  # allows us to run mixcord as a standalone
  def handle_event(_data, _state), do: :ok

end
