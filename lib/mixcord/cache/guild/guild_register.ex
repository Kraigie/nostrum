defmodule Mixcord.Cache.Guild.GuildRegister do
  @moduledoc false

  alias Mixcord.Cache.Guild.GuildServer
  alias Mixcord.Struct.Guild
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
        raise(Mixcord.Error.CacheError, "No entry in guild registry for id #{id}")
    end
  end

  def create_guild_process(id, guild) do
    case Supervisor.start_child(GuildSupervisor, [id, guild]) do
      {:ok, _pid} ->
        # Expected to return a tuple as all other methods do in the GuildServer module
        # This must return a similar structure to GuildServer.call
        {:ok, {Guild.to_struct(guild)}}
      other ->
        other
    end
  end

  def create_worker_spec(id, guild) do
    Spec.worker(GuildServer, [id, guild], id: id)
  end
end
