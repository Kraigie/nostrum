defmodule Mixcord.Cache.Guild.Supervisor do
  @moduledoc false

  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: Guild.Supervisor)
  end

  def init(_args) do
    supervise([], strategy: :one_for_one)
  end
end
