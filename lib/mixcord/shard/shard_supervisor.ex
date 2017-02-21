defmodule Mixcord.Shard.ShardSupervisor do
  @moduledoc false

  use Supervisor
  alias Mixcord.Shard

  def start_link(token, num_shards \\ 1) do
    Supervisor.start_link(__MODULE__, [token: token, num_shards: num_shards], name: ShardSupervisor)
  end

  def update_status(status, game) do
    Shard.Supervisor
      |> Supervisor.which_children
      |> Enum.map(fn {_id, pid, _type, _modules} -> Shard.update_status(pid, status, game) end)
  end

  @doc false
  def init(options) do
    children = for i <- 0..options[:num_shards] - 1, do: create_worker(options[:token], i)
    with_registry = [supervisor(Registry, [:duplicate, ProducerRegistry])] ++ children
    supervise(with_registry, strategy: :one_for_one, max_restarts: 3, max_seconds: 60)
  end

  @doc false
  def create_worker(token, shard_num) do
    # TODO: Add shard struct to map here with PID
    worker(Shard, [token, shard_num], [id: shard_num])
  end

end
