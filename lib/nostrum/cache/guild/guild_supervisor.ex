defmodule Nostrum.Cache.Guild.GuildSupervisor do
  @moduledoc false

  use Supervisor

  alias Nostrum.Cache.Guild.GuildServer

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: GuildSupervisor)
  end

  def init([]) do
    Supervisor.init([GuildServer.child_spec()], strategy: :simple_one_for_one)
  end
end
