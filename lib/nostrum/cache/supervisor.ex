defmodule Nostrum.Cache.Supervisor do
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

  def start_link(%{name: _name} = bot_options) do
    Supervisor.start_link(__MODULE__, bot_options)
  end

  def init(%{name: name} = _bot_options) do
    children = [
      Nostrum.Cache.Me,
      # Uses the configured cache implementations.
      {Nostrum.Cache.ChannelGuildMapping, name: name},
      {Nostrum.Cache.GuildCache, name: name},
      {Nostrum.Cache.MemberCache, name: name},
      {Nostrum.Cache.MessageCache, name: name},
      {Nostrum.Cache.UserCache, name: name},
      {Nostrum.Cache.PresenceCache, name: name}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
