defmodule Nostrum.Cache.PresenceCache.ETS do
  @tablename :presences
  @moduledoc """
  ETS-based cache for user presences.

  The ETS table name associated with the User Cache is `#{@tablename}`. Besides
  the methods provided below you can call any other ETS methods on the table.
  If you need to access the name of the presence cache's table
  programmatically, use the `tabname/0` function instead of hardcoding it in
  your application.

  ## Example
  ```elixir
  info = :ets.info(#{@tablename})
  [..., heir: :none, name: #{@tablename}, size: 1, ...]
  size = info[:size]
  1
  ```
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.PresenceCache

  alias Nostrum.Struct.{Guild, User}
  import Nostrum.Snowflake, only: [is_snowflake: 1]
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the cache's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(@tablename, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Return the ETS table name used for this cache."
  @spec tabname :: atom()
  def tabname do
    @tablename
  end

  @impl Nostrum.Cache.PresenceCache
  @doc "Retrieves a presence for a user from the cache by guild and id."
  @spec get(User.id(), Guild.id()) :: {:error, :presence_not_found} | {:ok, map}
  def get(user_id, guild_id) when is_snowflake(user_id) and is_snowflake(guild_id) do
    case :ets.lookup(@tablename, {user_id, guild_id}) do
      [] -> {:error, :presence_not_found}
      [{{^user_id, ^guild_id}, presence}] -> {:ok, presence}
    end
  end

  @impl Nostrum.Cache.PresenceCache
  @doc "Add the given presence data to the cache."
  @spec create(map) :: :ok
  def create(presence) do
    :ets.insert(@tablename, {{presence.user.id, presence.guild_id}, presence})
    :ok
  end

  @impl Nostrum.Cache.PresenceCache
  @doc "Update the given presence data in the cache."
  @spec update(map) :: {Guild.id(), nil | map, map} | :noop
  def update(presence) do
    case get(presence.user.id, presence.guild_id) do
      {:ok, p} ->
        new_presence = Map.merge(p, presence)
        create(new_presence)

        if p.activities == new_presence.activities and p.status == new_presence.status,
          do: :noop,
          else: {presence.guild_id, p, new_presence}

      {:error, _} ->
        create(presence)
        {presence.guild_id, nil, presence}
    end
  end

  @impl Nostrum.Cache.PresenceCache
  @doc "Bulk create multiple presences in the cache."
  @spec bulk_create(Guild.id(), [map]) :: :ok
  def bulk_create(_, []), do: :ok

  def bulk_create(guild_id, presences) when is_list(presences) do
    Enum.each(presences, fn p ->
      :ets.insert(@tablename, {{p.user.id, guild_id}, p})
    end)
  end
end
