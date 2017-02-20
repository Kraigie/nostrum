defmodule Mixcord.Cache.CacheSupervisor do
  @moduledoc false

  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: CacheSupervisor)
  end

  def empty_cache do
    Cache.Supervisor
      |> Supervisor.which_children
      |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.restart_child(Cache.Supervisor, pid) end)
  end

  def init(_args) do
    children = [
      # TODO: If shard dies, should guilds die also? An attempt will be made to restart them
      supervisor(Registry, [:unique, GuildRegistry]),
      supervisor(Mixcord.Cache.Guild.GuildSupervisor, []),
      worker(Mixcord.Cache.ChannelCache, []),
      worker(Mixcord.Cache.UserCache, [])
    ]

    supervise(children, strategy: :one_for_one)
  end

end
