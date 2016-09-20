defmodule Mixcord.Shard.Supervisor do
  use Supervisor

  def start_link(token, num_shards // 1) do
    Supervisor.start_link(__MODULE__, [token: token, num_shards: num_shards])
  end

  def update_status(status) do
    #for shard in shard cache, shard.updateStatus(cache.pid, status)
  end

  @doc false
  def init(options) do
    children = for i <- 1..options[:num_shards], do: create_worker(i, options[:token])
    supervise(children, strategy: :one_for_one)
  end

  @doc false
  def create_worker(shard_num, token) do
    #TODO: Add shard struct to map here with PID
    worker(Mixbot.Shard, [%{handler: __MODULE__, shard_num: i, token: options[:token]}])
  end
end