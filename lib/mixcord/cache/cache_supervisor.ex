defmodule Mixcord.Cache.Supervisor do
  @moduledoc false

  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: CacheSupervisor)
  end

  def empty_cache do
    CacheSupervisor
      |> Supervisor.which_children
      |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.restart_child(CacheSupervisor, pid) end)
  end

  def init(_args) do
    children = [
      worker(Mixcord.Cache.Guilds, [])
    ]

    supervise(children, strategy: :one_for_one)
  end

end