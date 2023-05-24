if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.MemberCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for guild members.

    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](State.md) documentation
    for more information.

    To retrieve the table name used by this cache, use `table/0`.
    """
    @moduledoc since: "0.8.0"

    @table_name :nostrum_guild_members
    @record_name @table_name

    @behaviour Nostrum.Cache.MemberCache

    alias Nostrum.Cache.MemberCache
    alias Nostrum.Struct.Guild
    alias Nostrum.Struct.Guild.Member
    alias Nostrum.Util
    use Supervisor

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = [
        attributes: [:key, :guild_id, :user_id, :data],
        index: [:guild_id, :user_id],
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

    @impl MemberCache
    @doc "Add the given member to the given guild in the cache."
    @spec create(Guild.id(), map()) :: Member.t()
    def create(guild_id, payload) do
      member = Member.to_struct(payload)
      record = {@record_name, {guild_id, member.user_id}, guild_id, member.user_id, member}
      writer = fn -> :mnesia.write(record) end
      {:atomic, :ok} = :mnesia.sync_transaction(writer)
      member
    end

    @impl MemberCache
    @doc "Update the given member for the given guild in the cache."
    @spec update(Guild.id(), map()) :: {Guild.id(), Member.t() | nil, Member.t()}
    def update(guild_id, payload) do
      new_member = Util.cast(payload, {:struct, Member})
      key = {guild_id, new_member.user_id}

      old_member =
        :mnesia.activity(:sync_transaction, fn ->
          case :mnesia.read(@table_name, key, :write) do
            [{_tag, _key, _guild_id, _user_id, old_member} = entry] ->
              :mnesia.write(put_elem(entry, 4, new_member))
              old_member

            [] ->
              nil
          end
        end)

      {guild_id, old_member, new_member}
    end

    @impl MemberCache
    @doc "Remove the given member from the given guild in the cache."
    @spec delete(Guild.id(), Member.user_id()) :: {Guild.id(), Member.t()} | :noop
    def delete(guild_id, user_id) do
      key = {guild_id, user_id}

      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, key, :write) do
          [{_tag, _key, _guild_id, _user_id, member}] ->
            :mnesia.delete(@table_name, key, :write)
            {guild_id, member}

          _ ->
            :noop
        end
      end)
    end

    @impl MemberCache
    @doc "Bulk create a chunk of members for the given guild in the cache."
    def bulk_create(guild_id, members) do
      :mnesia.activity(:sync_transaction, fn ->
        # https://erlang.org/pipermail/erlang-questions/2005-August/016382.html
        # Substantially reduces locking overhead for large amount of records.
        :mnesia.write_lock_table(@table_name)

        Enum.each(
          members,
          &:mnesia.write(
            {@record_name, {guild_id, &1.user.id}, guild_id, &1.user.id,
             Util.cast(&1, {:struct, Member})}
          )
        )
      end)

      true
    end

    @impl MemberCache
    @doc "Get a QLC query handle for the member cache."
    @spec query_handle :: :qlc.query_handle()
    def query_handle do
      :mnesia.table(@table_name)
    end

    @impl MemberCache
    @doc "Wrap QLC operations in a transaction."
    @doc since: "0.8.0"
    def wrap_qlc(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
