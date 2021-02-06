defmodule Nostrum.Cache.Guild.GuildSupervisor do
  @moduledoc false

  use DynamicSupervisor

  alias Nostrum.Cache.Guild.GuildServer

  def start_link([_gateway, _shard_num] = opts) do
    DynamicSupervisor.start_link(__MODULE__, opts)
  end

  def start_child(id, guild, shard_id) do
    shard_guild_sup =
      ShardSupervisor
      |> Supervisor.which_children()
      |> Stream.filter(fn {_id, _pid, _type, [modules]} -> modules == Nostrum.Shard end)
      |> Stream.filter(fn {id, _pid, _type, _modules} -> id == shard_id end)
      |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.which_children(pid) end)
      |> List.flatten()
      |> Enum.filter(fn {_id, _pid, _type, [modules]} -> modules == __MODULE__ end)
      |> List.first()
      |> elem(1)

    spec = {GuildServer, id: id, guild: guild}
    DynamicSupervisor.start_child(shard_guild_sup, spec)
  end

  def init([_gateway, shard_num]) do
    DynamicSupervisor.init(strategy: :one_for_one, name: {__MODULE__, {:shard, shard_num}})
  end
end
