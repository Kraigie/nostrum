defmodule Nostrum.Shard do
  @moduledoc false

  use Supervisor

  alias Nostrum.Shard.Session

  def start_link(gateway, token, shard_num) do
    args = %{
      gw: gateway,
      token: token,
      shard_num: shard_num
    }

    Supervisor.start_link(__MODULE__, args, name: :"Shard-#{shard_num}")
  end

  def init(options) do
    children = [
      {Session, [options]}
      # TODO: Add per shard ratelimiter
      # TODO: Add per shard cache
    ]

    Supervisor.init(children, strategy: :one_for_all, max_restarts: 3, max_seconds: 60)
  end
end
