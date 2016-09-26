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
    children = for i <- 1..options[:num_shards], do: create_worker(options[:token], options[:caller], i)
    supervise(children, strategy: :one_for_one)
  end

  @doc false
  def create_worker(shard_num, caller, token) do
    #TODO: Add shard struct to map here with PID
    worker(Mixcord.Shard, [token: token, caller: caller, shard_num: shard_num], [id: shard_num])
  end

end