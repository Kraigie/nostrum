defmodule Nostrum.Cache.CacheSupervisor do
  @moduledoc """
  Supervises caches for nostrum structures.

  See the documentation for the relevant submodules for details:

  - `Nostrum.Cache.ChannelCache`
  - `Nostrum.Cache.GuildCache`
  - `Nostrum.Cache.Me`
  - `Nostrum.Cache.PresenceCache`
  - `Nostrum.Cache.UserCache`
  """

  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: CacheSupervisor)
  end

  def init([]) do
    children = [
      Nostrum.Cache.Me,
      # Uses the configured cache implementation.
      Nostrum.Cache.GuildCache
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
