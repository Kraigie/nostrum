if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.UserCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for users.

    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](State.md) documentation
    for more information.

    To retrieve the table name used by this cache, use `table/0`.
    """
    @moduledoc since: "0.8.0"

    @table_name :nostrum_users
    @record_name @table_name

    @behaviour Nostrum.Cache.UserCache

    alias Nostrum.Cache.UserCache
    alias Nostrum.Struct.User
    use Supervisor

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = [
        attributes: [:id, :data],
        record_name: @record_name
      ]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @doc "Retrieve the Mnesia table name used for the cache."
    @spec table :: atom()
    def table, do: @table_name

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(@table_name)

    @doc "Clear any objects in the cache."
    @spec clear() :: :ok
    def clear do
      {:atomic, :ok} = :mnesia.clear_table(@table_name)
      :ok
    end

    # Used by dispatch

    @impl UserCache
    @doc "Create a user from upstream data."
    @spec create(map()) :: User.t()
    def create(payload) do
      user = User.to_struct(payload)
      record = {@record_name, user.id, user}
      writer = fn -> :mnesia.write(record) end
      :ok = :mnesia.activity(:sync_transaction, writer)
      user
    end

    @impl UserCache
    @doc "Bulk create a chunk of users in the cache."
    def bulk_create(users) do
      :mnesia.activity(:sync_transaction, fn ->
        # https://erlang.org/pipermail/erlang-questions/2005-August/016382.html
        # Substantially reduces locking overhead for large amount of records.
        :mnesia.write_lock_table(@table_name)

        Enum.each(users, &:mnesia.write({@record_name, &1.id, User.to_struct(&1)}))
      end)

      :ok
    end

    @impl UserCache
    @doc "Update the given member for the given guild in the cache."
    @spec update(map()) :: {User.t() | nil, User.t()}
    def update(payload) do
      new_user = User.to_struct(payload)

      old_user =
        :mnesia.activity(:sync_transaction, fn ->
          case :mnesia.read(@table_name, new_user.id, :write) do
            [{_tag, _id, old_user} = entry] ->
              :mnesia.write(put_elem(entry, 2, new_user))
              old_user

            [] ->
              nil
          end
        end)

      {old_user, new_user}
    end

    @impl UserCache
    @doc "Remove the given user from cache."
    @spec delete(User.id()) :: User.t() | :noop
    def delete(user_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, user_id, :write) do
          [{_tag, _user_id, user}] ->
            :mnesia.delete(@table_name, user_id, :write)
            user

          _ ->
            :noop
        end
      end)
    end

    @impl UserCache
    @doc "Get a QLC query handle for the user cache."
    @spec query_handle :: :qlc.query_handle()
    def query_handle do
      :mnesia.table(@table_name)
    end

    @impl UserCache
    @doc "Wrap QLC operations in a transaction."
    def wrap_qlc(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
