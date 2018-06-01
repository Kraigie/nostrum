defmodule Nostrum.Cache.CacheSupervisor do
  @moduledoc false

  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: CacheSupervisor)
  end

  def init([]) do
    children = [
      # REVIEW: If shard dies, should guilds die also? An attempt will be made to restart them
      {Registry, keys: :unique, name: GuildRegistry},
      Nostrum.Cache.Guild.GuildSupervisor,
      Nostrum.Cache.Me
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
