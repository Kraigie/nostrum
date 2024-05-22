defmodule Nostrum.Shard do
  @moduledoc false

  use Supervisor, restart: :transient

  alias Nostrum.Shard.Session

  def start_link({:connect, [_, shard_num, _total]} = opts) do
    Supervisor.start_link(__MODULE__, opts, name: :"Nostrum.Shard-#{shard_num}")
  end

  def start_link(
        {:reconnect,
         %{
           shard_num: shard_num,
           total_shards: _total_shards,
           gateway: _gateway,
           resume_gateway: _resume_gateway,
           seq: _seq,
           session: _session
         }} =
          opts
      ) do
    Supervisor.start_link(__MODULE__, opts, name: :"Nostrum.Shard-#{shard_num}")
  end

  def start_link([_, _shard_num, _total] = opts) do
    start_link({:connect, opts})
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
