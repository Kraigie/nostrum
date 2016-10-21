defmodule Mixcord.Shard.Supervisor do
  @moduledoc false

  use Supervisor
  alias Mixcord.Shard

  def start_link(token, caller, num_shards \\ 1) do
    Supervisor.start_link(__MODULE__, [token: token, caller: caller, num_shards: num_shards], name: Shard.Supervisor)
  end

  def update_status(idle, game) do
    Shard.Supervisor
      |> Supervisor.which_children
      |> Enum.map(fn {_id, pid, _type, _modules} -> Shard.update_status(pid, {idle, game}) end)
  end

  @doc false
  def init(options) do
    children = for i <- 0..options[:num_shards] - 1, do: create_worker(options[:token], options[:caller], i)
    supervise(children, strategy: :one_for_one)
  end

  @doc false
  def create_worker(token, caller, shard_num) do
    # TODO: Add shard struct to map here with PID
    worker(Shard, [token, caller, shard_num], [id: shard_num])
  end

end