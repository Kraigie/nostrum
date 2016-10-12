defmodule Mixcord.Cache.Supervisor do
  @moduledoc false

  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, [], name: CacheSupervisor)
  end

  def init(_args) do
    children = [
      worker(Mixcord.Cache.Guilds, [])
    ]

    supervise(children, strategy: :one_for_one)
  end

end