if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.UserCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for users.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    @base_table_name :nostrum_users

    @behaviour Nostrum.Cache.UserCache

    alias Nostrum.Bot
    alias Nostrum.Cache.UserCache
    alias Nostrum.Snowflake
    alias Nostrum.Struct.User
    use Supervisor

    @doc "Start the supervisor."
    def start_link(opts) do
      Supervisor.start_link(__MODULE__, opts)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(opts) do
      table_name = :"#{@base_table_name}_#{Keyword.fetch!(opts, :name)}"

      table_options = [
        attributes: [:id, :data],
        record_name: record_name()
      ]

      case :mnesia.create_table(table_name, table_options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @doc "Retrieve the Mnesia table name used for the cache."
    @spec table :: atom()
    def table, do: :"#{@base_table_name}_#{Bot.fetch_bot_name()}"

    defp record_name, do: :nostrum_user

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(table())

    @doc "Clear any objects in the cache."
    @spec clear() :: :ok
    def clear do
      {:atomic, :ok} = :mnesia.clear_table(table())
      :ok
    end

    @doc "Retrieve a user from the cache."
    @impl UserCache
    @spec get(User.id()) :: {:ok, User.t()} | {:error, :user_not_found}
    def get(user_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(table(), user_id) do
          [{_tag, _id, user}] ->
            {:ok, user}

          _ ->
            {:error, :user_not_found}
        end
      end)
    end

    # Used by dispatch

    @impl UserCache
    @doc "Create a user from upstream data."
    @spec create(map()) :: User.t()
    def create(payload) do
      user = User.to_struct(payload)
      record = {record_name(), user.id, user}
      writer = fn -> :mnesia.write(table(), record, :write) end
      :ok = :mnesia.activity(:sync_transaction, writer)
      user
    end

    @impl UserCache
    @doc "Bulk create a chunk of users in the cache."
    def bulk_create(users) do
      :mnesia.activity(:sync_transaction, fn ->
        # https://erlang.org/pipermail/erlang-questions/2005-August/016382.html
        # Substantially reduces locking overhead for large amount of records.
        :mnesia.write_lock_table(table())

        Enum.each(
          users,
          &:mnesia.write(table(), {record_name(), &1.id, User.to_struct(&1)}, :write)
        )
      end)

      :ok
    end

    @impl UserCache
    @doc "Update a User if it exists in the cache."
    @spec update(map()) :: {User.t() | nil, User.t()}
    def update(payload) do
      # We don't know if the user_id is an atom or a string here.
      user_id =
        (Map.get(payload, :id) || Map.get(payload, "id"))
        |> Snowflake.cast!()

      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(table(), user_id, :write) do
          [{_tag, _id, old_user} = entry] ->
            new_user = User.to_struct(payload, old_user)

            :mnesia.write(table(), put_elem(entry, 2, new_user), :write)
            {old_user, new_user}

          [] ->
            {nil, User.to_struct(payload)}
        end
      end)
    end

    @impl UserCache
    @doc "Remove the given user from cache."
    @spec delete(User.id()) :: User.t() | :noop
    def delete(user_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(table(), user_id, :write) do
          [{_tag, _user_id, user}] ->
            :mnesia.delete(table(), user_id, :write)
            user

          _ ->
            :noop
        end
      end)
    end

    @impl UserCache
    @doc "Wrap QLC operations in a transaction."
    def wrap_query(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
