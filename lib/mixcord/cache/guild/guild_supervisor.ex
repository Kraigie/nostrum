defmodule Mixcord.Cache.Guild.GuildSupervisor do
  @moduledoc false

  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: GuildSupervisor)
  end

  def init(_args) do
    supervise([], strategy: :one_for_one)
  end
end
