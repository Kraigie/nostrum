defmodule Mixcord.Shard.Supervisor do
  use Supervisor

  def start_link(token, caller, num_shards \\ 1) do
    Supervisor.start_link(__MODULE__, [token: token, caller: caller, num_shards: num_shards])
  end

  def update_status(_status) do
    #for shard in shard cache, shard.updateStatus(cache.pid, status)
  end

  @doc false
  def init(options) do
    children = for i <- 1..options[:num_shards], do: create_worker(i, options[:caller], options[:token])
    supervise(children, strategy: :one_for_one)
  end

  @doc false
  def create_worker(shard_num, caller, token) do
    #TODO: Add shard struct to map here with PID
    worker(Mixcord.Shard, [caller: caller, shard_num: shard_num, token: token], [id: shard_num])
  end

end