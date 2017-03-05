defmodule Nostrum.Cache.Guild.GuildSupervisor do
  @moduledoc false

  use Supervisor
  
  alias Nostrum.Cache.Guild.GuildServer

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: GuildSupervisor)
  end

  def init(_args) do
    children = [
      worker(GuildServer, [], restart: :transient)
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end
