defmodule Mixcord.Cache.Guild.Supervisor do
  @moduledoc false

  use Supervisor
  require Logger

  def start_link do
    Supervisor.start_link(__MODULE__, [], name: Guild.Supervisor)
  end

  def init(_args) do
    supervise([], strategy: :one_for_one)
  end

  def add_guild(id, guild) do
    IO.inspect "ADDING GUILD"
    case Supervisor.start_child(Guild.Supervisor, worker(Mixcord.Cache.Guild, [])) do
      {:ok, child} ->
        child
      {:error, reason} ->
        #Logger.warn "Could not start a new guild process with id #{id}, reason: #{inspect reason}"
        Logger.warn inspect reason
    end
  end
end
