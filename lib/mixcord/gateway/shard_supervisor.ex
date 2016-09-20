defmodule Mixcord.ShardSupervisor do
  use Supervisor

  def start_link(token, num_shards // 1) do
    Supervisor.start_link(__MODULE__, %{token: token, num_shards: num_shards})
  end

  def init(options) do
    children = for i <- 1..options.num_shards do
      worker(Mixbot.Shard, [%{handler: __MODULE__, shard_num: i, token: options.token}])
     end

     supervise(children, strategy: :one_for_one)
  end
end