defmodule Nostrum.Cache.CacheSupervisor do
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
      # REVIEW: If shard dies, should guilds die also? An attempt will be made to restart them
      supervisor(Registry, [:unique, GuildRegistry]),
      supervisor(Nostrum.Cache.Guild.GuildSupervisor, []),
      worker(Nostrum.Cache.Me, []),
      worker(Nostrum.Cache.ChannelCache, []),
      worker(Nostrum.Cache.UserCache, [])
    ]

    supervise(children, strategy: :one_for_one)
  end

end
