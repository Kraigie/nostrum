defmodule Nostrum.Cache.Guild.GuildRegister do
  @moduledoc false

  alias Nostrum.Cache.Guild.GuildServer
  alias Supervisor.Spec

  def lookup(id) do
    case Registry.lookup(GuildRegistry, id) do
      [{pid, _}] ->
        {:ok, pid}
      [] ->
        {:error, :id_not_found_on_guild_lookup}
    end
  end

  def lookup!(id) do
    case Registry.lookup(GuildRegistry, id) do
      [{pid, _}] ->
        pid
      [] ->
        raise(Nostrum.Error.CacheError, "No entry in guild registry for id #{id}")
    end
  end

  def create_guild_process(id, guild) do
    case Supervisor.start_child(GuildSupervisor, [id, guild]) do
      {:ok, _pid} ->
        {:ok, guild}
      other ->
        other
    end
  end
  
end
