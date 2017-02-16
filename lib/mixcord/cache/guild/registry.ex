defmodule Mixcord.Cache.Guild.Registry do
  @moduledoc false

  alias Mixcord.Cache
  alias Mixcord.Struct
  alias Supervisor.Spec

  # TODO: Refactor this nasty code and potentially move
  @doc false
  def create_guild_process!(id, guild) do
    case Supervisor.start_child(Guild.Supervisor, create_worker_spec(id, guild)) do
      {:error, {:already_started, pid}} ->
        handle_guild_unavailability!(pid, guild)
      {:error, reason} ->
        raise(Mixcord.Error.CacheError,
          "Could not start a new guild process with id #{id}, reason: #{inspect reason}")
      other ->
        Struct.Guild.to_struct(guild)
    end
  end

  def create_worker_spec(id, guild) do
    Spec.worker(Cache.Guild, [id, guild], id: id)
  end

  @doc """
  When attempting to start a guild, if the guild is already created, check to see
  if it's unavailable, and if it is replace it with the new guild payload.
  """
  defp handle_guild_unavailability!(pid, guild) do
    if Cache.Guild.unavailable?(pid) do
      Cache.Guild.update_guild(pid, guild)
    else
      raise(Mixcord.Error.CacheError, "Attempted to create a child with an id that already exists")
    end
  end

end
