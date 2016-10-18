defmodule Mixcord.Cache.Supervisor do
  @moduledoc false

  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: Cache.Supervisor)
  end

  def empty_cache do
    Cache.Supervisor
      |> Supervisor.which_children
      |> Enum.map(fn {_id, pid, _type, _modules} -> Supervisor.restart_child(Cache.Supervisor, pid) end)
  end

  def init(_args) do
    children = [
      worker(Mixcord.Cache.Guild, [])
    ]

    supervise(children, strategy: :one_for_one)
  end

end