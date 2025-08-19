defmodule Nostrum.Shard do
  @moduledoc false

  use Supervisor, restart: :permanent

  alias Nostrum.Shard.Session

  def start_link(
        %{
          shard_num: _shard_num,
          total_shards: _total_shards,
          gateway: _gateway,
          bot_options: _bot_options
        } = opts
      ) do
    Supervisor.start_link(__MODULE__, opts)
  end

  def init(opts) do
    children = [
      {Session, opts}
      # TODO: Add per shard ratelimiter
      # TODO: Add per shard cache
    ]

    Supervisor.init(children,
      strategy: :one_for_all,
      max_restarts: 3,
      max_seconds: 60,
      auto_shutdown: :any_significant
    )
  end
end
