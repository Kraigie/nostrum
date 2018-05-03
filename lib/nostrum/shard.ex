defmodule Nostrum.Shard do
  @moduledoc false

  use Supervisor

  alias Nostrum.Shard.Session

  def start_link([_, shard_num] = opts) do
    Supervisor.start_link(__MODULE__, opts, name: :"Shard-#{shard_num}")
  end

  def init(opts) do
    children = [
      {Session, opts}
      # TODO: Add per shard ratelimiter
      # TODO: Add per shard cache
    ]

    Supervisor.init(children, strategy: :one_for_all, max_restarts: 3, max_seconds: 60)
  end
end
