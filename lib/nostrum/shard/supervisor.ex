defmodule Nostrum.Shard.Supervisor do
  @moduledoc false

  use Supervisor

  alias Nostrum.{Shard, Util}
  alias Nostrum.Shard.Session
  alias Nostrum.Shard.Stage.{Cache, Producer}

  require Logger

  def start_link([token, :auto]) do
    {url, shards} = Util.gateway()

    Supervisor.start_link(
      __MODULE__,
      [url, token, shards],
      name: ShardSupervisor
    )
  end

  def start_link([token, num_shards]) do
    {url, shards} = Util.gateway()

    if num_shards != shards,
      do:
        Logger.info(
          "Specified #{num_shards} shards " <>
            "when the recommended number is #{shards}. Consider using the num_shards: " <>
            ":auto option in your Nostrum config."
        )

    Supervisor.start_link(
      __MODULE__,
      [url, token, num_shards],
      name: ShardSupervisor
    )
  end

  def update_status(status, game, stream, type) do
    ShardSupervisor
    |> Supervisor.which_children()
    |> Enum.filter(fn {_id, _pid, _type, [modules]} -> modules == Nostrum.Shard end)
    |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.which_children(pid) end)
    |> List.flatten()
    |> Enum.map(fn {_id, pid, _type, _modules} ->
      Session.update_status(pid, status, game, stream, type)
    end)
  end

  @doc false
  def init([url, token, num_shards]) do
    children =
      [
        Producer,
        Cache
      ] ++ for i <- 0..(num_shards - 1), do: create_worker(url, token, i)

    Supervisor.init(children, strategy: :one_for_one, max_restarts: 3, max_seconds: 60)
  end

  @doc false
  def create_worker(gateway, token, shard_num) do
    Supervisor.child_spec(
      {Shard, [gateway, shard_num, token]},
      id: shard_num
    )
  end
end
