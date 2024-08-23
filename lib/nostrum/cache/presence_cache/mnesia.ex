if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.PresenceCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for presences.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    @table_name :nostrum_presences
    @record_name @table_name

    @behaviour Nostrum.Cache.PresenceCache

    alias Nostrum.Cache.PresenceCache

    alias Nostrum.Struct.Guild
    alias Nostrum.Struct.User

    use Supervisor

    @doc "Retrieve the table name used by the cache."
    @spec table :: atom()
    def table, do: @table_name

    @doc "Clear any objects in the cache."
    @spec clear() :: {:atomic, :ok} | {:aborted, term()}
    def clear, do: :mnesia.clear_table(@table_name)

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(@table_name)

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = [attributes: [:key, :data], record_name: @record_name]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @impl PresenceCache
    @doc "Retrieve a presence from the cache."
    @spec get(Guild.id(), User.id()) :: {:ok, PresenceCache.presence()} | {:error, any}
    def get(guild_id, user_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, {guild_id, user_id}) do
          [{_tag, _key, presence}] -> {:ok, presence}
          [] -> {:error, :presence_not_found}
        end
      end)
    end

    @impl PresenceCache
    @doc "Add the given presence to the cache."
    @spec create(map()) :: :ok
    def create(presence) do
      record = {@record_name, {presence.guild_id, presence.user.id}, presence}
      :ok = :mnesia.activity(:sync_transaction, fn -> :mnesia.write(record) end)
    end

    @impl PresenceCache
    @doc "Update the given presence in the cache."
    @spec update(map()) :: {Guild.id(), presence | nil, presence} | :noop
          when presence: PresenceCache.presence()
    def update(new) do
      key = {new.guild_id, new.user.id}

      :mnesia.activity(:sync_transaction, fn ->
        new_activities = new[:activities]
        new_status = new[:status]

        case :mnesia.read(@table_name, key, :write) do
          [{_tag, _key, %{activities: ^new_activities, status: ^new_status}}] ->
            :noop

          [{_tag, _key, old}] ->
            merged = Map.merge(old, new)
            :mnesia.write(@table_name, {@record_name, key, merged}, :write)
            {new.guild_id, old, merged}

          [] ->
            {new.guild_id, nil, new}
        end
      end)
    end

    @impl PresenceCache
    @doc "Bulk create a chunk of presences for the given guild in the cache."
    def bulk_create(guild_id, presences) do
      :mnesia.activity(:sync_transaction, fn ->
        # https://erlang.org/pipermail/erlang-questions/2005-August/016382.html
        # Substantially reduces locking overhead for large amount of records.
        :mnesia.write_lock_table(@table_name)

        Enum.each(
          presences,
          &:mnesia.write({@record_name, {guild_id, &1.user.id}, &1})
        )
      end)
    end

    @impl PresenceCache
    @doc "Wrap query operations in a transaction."
    @doc since: "0.8.0"
    def wrap_query(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
