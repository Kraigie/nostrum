defmodule Nostrum.Shard do
  @moduledoc false

  use Supervisor, restart: :transient

  alias Nostrum.Shard.Session

  def start_link({:connect, [_, shard_num, _total, _bot_options]} = opts) do
    Supervisor.start_link(__MODULE__, opts, name: :"Nostrum.Shard-#{shard_num}")
  end

  def start_link(
        {:reconnect,
         %{
           consumer: _consumer,
           shard_num: shard_num,
           total_shards: _total_shards,
           gateway: _gateway,
           resume_gateway: _resume_gateway,
           seq: _seq,
           session: _session,
           wrapped_token: _wrapped_token
         }} =
          opts
      ) do
    Supervisor.start_link(__MODULE__, opts, name: :"Nostrum.Shard-#{shard_num}")
  end

  def start_link([_, _shard_num, _total, _bot_options] = opts) do
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
