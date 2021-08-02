defmodule Nostrum.Cache.CacheSupervisor do
  @moduledoc false

  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: CacheSupervisor)
  end

  def init([]) do
    children = [
      Nostrum.Cache.Me
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
