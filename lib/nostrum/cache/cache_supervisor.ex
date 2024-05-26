defmodule Nostrum.Cache.CacheSupervisor do
  @moduledoc """
  Supervises caches for nostrum structures.

  See the documentation for the relevant submodules for details:

  - `Nostrum.Cache.GuildCache`
  - `Nostrum.Cache.Me`
  - `Nostrum.Cache.MemberCache`
  - `Nostrum.Cache.PresenceCache`
  - `Nostrum.Cache.UserCache`
  """

  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      Nostrum.Cache.Me,
      # Uses the configured cache implementations.
      Nostrum.Cache.ChannelGuildMapping,
      Nostrum.Cache.GuildCache,
      Nostrum.Cache.MemberCache,
      Nostrum.Cache.MessageCache,
      Nostrum.Cache.UserCache,
      Nostrum.Cache.PresenceCache
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
