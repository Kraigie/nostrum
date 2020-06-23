defmodule Nostrum.Cache.Guild.GuildRegister do
  @moduledoc false

  alias Nostrum.Cache.Guild.GuildSupervisor
  alias Nostrum.Cache.GuildCache

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
    case GuildSupervisor.start_child(id, guild) do
      {:ok, _pid} ->
        {:ok, guild}

      {:error, {:already_registered, _pid}} ->
        GuildCache.get(id)

      other ->
        other
    end
  end
end
