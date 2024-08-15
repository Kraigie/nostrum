if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.MemberCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for guild members.

    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](functionality/state.md)
    documentation for more information.

    To retrieve the table name used by this cache, use `table/0`.
    """
    @moduledoc since: "0.8.0"

    @table_name :nostrum_guild_members
    @record_name @table_name

    @behaviour Nostrum.Cache.MemberCache

    alias Nostrum.Cache.MemberCache
    alias Nostrum.Snowflake
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
      # Force keys to be atoms before casting just to simplify finding the user_id
      # because of the atom/string ambiguity issues from the gateway that Discord
      # won't fix.

      member_payload = Map.new(payload, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

      member_id = Util.cast(member_payload[:user][:id], Snowflake)

      key = {guild_id, member_id}

      {old_member, new_member} =
        :mnesia.activity(:sync_transaction, fn ->
          case :mnesia.read(@table_name, key, :write) do
            [{_tag, _key, _guild_id, _user_id, old_member} = entry] ->
              new_member = Member.to_struct(member_payload, old_member)

              :mnesia.write(put_elem(entry, 4, new_member))
              {old_member, new_member}

            [] ->
              new_member = Member.to_struct(member_payload)
              {nil, new_member}
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
      query_handle([])
    end

    @doc """
    Retrieve a query handle for the table with optional match specification guards.

    ## Match specification variables

    - `$1`: The guild ID the presence is stored on
    - `$2`: The user associated with the presence
    - `$3`: The member itself
    """
    @doc since: "0.10.0"
    @spec query_handle([term()]) :: :qlc.query_handle()
    def query_handle(guards) do
      ms = [{{_name = :_, _key = :_, :"$1", :"$2", :"$3"}, guards, [{{{{:"$1", :"$2"}}, :"$3"}}]}]
      :mnesia.table(@table_name, {:traverse, {:select, ms}})
    end

    @impl MemberCache
    @doc "Wrap QLC operations in a transaction."
    @doc since: "0.8.0"
    def wrap_qlc(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
