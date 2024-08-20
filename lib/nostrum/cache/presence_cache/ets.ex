defmodule Nostrum.Cache.PresenceCache.ETS do
  @moduledoc """
  ETS-based cache for user presences.

  If you need to get the table reference for the table used by this module,
  please use the `table/0` function.
  """
  @moduledoc since: "0.5.0"
  @dialyzer {:nowarn_function, update: 1}

  alias Nostrum.Cache.PresenceCache

  @behaviour PresenceCache

  @table_name :nostrum_presences

  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.User

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
    _tid = :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl PresenceCache
  @doc "Retrieve a presence from the cache."
  @spec get(Guild.id(), User.id()) :: {:ok, PresenceCache.presence()} | {:error, any}
  def get(guild_id, user_id) do
    case :ets.lookup(@table_name, {guild_id, user_id}) do
      [{_, presence}] -> {:ok, presence}
      [] -> {:error, :presence_not_found}
    end
  end

  @impl PresenceCache
  @doc "Add the given presence data to the cache."
  @spec create(map) :: :ok
  def create(presence) do
    :ets.insert(@table_name, {{presence.guild_id, presence.user.id}, presence})
    :ok
  end

  @impl PresenceCache
  @doc "Update the given presence data in the cache."
  @spec update(map()) :: {Guild.id(), presence | nil, presence} | :noop
        when presence: PresenceCache.presence()
  def update(new) do
    case :ets.lookup(@table_name, {new.guild_id, new.user.id}) do
      [{{guild_id, _}, old}] ->
        merged = Map.merge(old, new)
        create(merged)

        if old.activities == merged.activities and old.status == merged.status,
          do: :noop,
          else: {guild_id, old, merged}

      [] ->
        create(new)
        {new.guild_id, nil, new}
    end
  end

  @impl PresenceCache
  @doc "Bulk create multiple presences in the cache."
  @spec bulk_create(Guild.id(), [map]) :: :ok
  def bulk_create(_, []), do: :ok

  def bulk_create(guild_id, presences) when is_list(presences) do
    Enum.each(presences, fn p ->
      :ets.insert(@table_name, {{guild_id, p.user.id}, p})
    end)
  end
end
