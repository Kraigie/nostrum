defmodule Nostrum.Cache.PresenceCache.ETS do
  @moduledoc """
  ETS-based cache for user presences.

  If you need to get the table reference for the table used by this module,
  please use the `table/0` function.
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.PresenceCache

  @table_name :nostrum_presences

  alias Nostrum.Struct.{Guild, User}
  import Nostrum.Snowflake, only: [is_snowflake: 1]
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Retrieve the ETS table reference used for the cache."
  @doc since: "0.8.0"
  @spec table :: :ets.table()
  def table, do: @table_name

  @doc "Set up the cache's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl Nostrum.Cache.PresenceCache
  @doc "Retrieves a presence for a user from the cache by guild and id."
  @spec get(User.id(), Guild.id()) :: {:error, :presence_not_found} | {:ok, map}
  def get(user_id, guild_id) when is_snowflake(user_id) and is_snowflake(guild_id) do
    case :ets.lookup(@table_name, {user_id, guild_id}) do
      [] -> {:error, :presence_not_found}
      [{{^user_id, ^guild_id}, presence}] -> {:ok, presence}
    end
  end

  @impl Nostrum.Cache.PresenceCache
  @doc "Add the given presence data to the cache."
  @spec create(map) :: :ok
  def create(presence) do
    :ets.insert(@table_name, {{presence.user.id, presence.guild_id}, presence})
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
      :ets.insert(@table_name, {{p.user.id, guild_id}, p})
    end)
  end
end
