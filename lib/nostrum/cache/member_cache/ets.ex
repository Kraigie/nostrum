defmodule Nostrum.Cache.MemberCache.ETS do
  @table_name :nostrum_members
  @moduledoc """
  An ETS-based cache for members.

  The supervisor defined by this module will set up the ETS table associated
  with it.

  The default table name under which guilds are cached is `#{@table_name}`. In
  addition to the cache behaviour implementations provided by this module, you
  can also call regular ETS table methods on it, such as `:ets.info`. Use the
  `tabname/0` function for retrieving the table name at runtime.

  Note that users should not call the functions not related to this specific
  implementation of the cache directly. Instead, call the functions of
  `Nostrum.Cache.MemberCache` directly, which will dispatch to the configured
  cache.
  """
  @moduledoc since: "0.7.0"

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

  @doc "Set up the cache's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(tabname(), [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the ETS table name used for the cache."
  @spec tabname :: atom()
  def tabname, do: @table_name

  # Used by clients

  @doc """
  Get all members for a given guild ID.

  Note that this implementation uses `:ets.safe_fixtable/2` to ensure safe
  traversal until the returned stream is exhausted or the calling process
  terminates. This also degrades performance for other table operations, and
  prevents table deletions from freeing up memory. It is therefore recommended
  to finish your business quickly.
  """
  @impl MemberCache
  @spec get(Guild.id()) :: Enumerable.t(Member.t())
  def get(guild_id) do
    safe_stream_matchspec([{{{:"$1", :"$2"}, :"$3"}, [{:==, :"$1", guild_id}], [:"$3"]}])
  end

  @doc "Fetch a single member from the cache."
  @impl MemberCache
  @spec get(Guild.id(), Member.user_id()) :: {:ok, Member.t()} | {:error, atom()}
  def get(guild_id, user_id) do
    case :ets.lookup(@table_name, {guild_id, user_id}) do
      [{_key, member}] -> {:ok, member}
      [] -> {:error, :not_found}
    end
  end

  @doc """
  Get all members for the given user ID.

  The same implementation notes as for `get/1` apply for this function.
  """
  @impl MemberCache
  @spec by_user(Member.user_id()) :: Enumerable.t({Guild.id(), Member.t()})
  def by_user(user_id) do
    safe_stream_matchspec([{{{:"$1", :"$2"}, :"$3"}, [{:==, :"$2", user_id}], [{{:"$1", :"$3"}}]}])
  end

  defp safe_stream_matchspec(ms) do
    Stream.resource(
      fn ->
        :ets.safe_fixtable(@table_name, true)
        []
      end,
      fn
        [] ->
          case :ets.select(@table_name, ms, 500) do
            # Empty table. Bail.
            :"$end_of_table" -> {:halt, nil}
            {_matches, _continuation} = result -> result
          end

        continuation ->
          case :ets.select(continuation) do
            # End of table. Bail.
            :"$end_of_table" -> {:halt, nil}
            {_matches, _continuation} = result -> result
          end
      end,
      fn _acc ->
        :ets.safe_fixtable(@table_name, false)
      end
    )
  end

  # Used by dispatch

  @doc "Add the given member to the given guild in the cache."
  @impl MemberCache
  @spec create(Guild.id(), map()) :: Member.t()
  def create(guild_id, payload) do
    member = Util.cast(payload, {:struct, Member})
    key = {guild_id, member.user_id}
    true = :ets.insert(@table_name, {key, member})
    member
  end

  @doc "Update the given member for the given guild in the cache."
  @impl MemberCache
  @spec update(Guild.id(), map()) :: {Guild.id(), Member.t() | nil, Member.t()}
  def update(guild_id, payload) do
    new_member = Util.cast(payload, {:struct, Member})

    case :ets.lookup(@table_name, {guild_id, new_member.user_id}) do
      [{key, old_member}] ->
        true = :ets.update_element(@table_name, key, {2, new_member})
        {guild_id, old_member, new_member}

      [] ->
        {guild_id, nil, new_member}
    end
  end

  @doc "Remove the given member from the given guild in the cache."
  @impl MemberCache
  @spec delete(Guild.id(), map()) :: {Guild.id(), Member.t()} | :noop
  def delete(guild_id, user) do
    case :ets.take(@table_name, {guild_id, user.id}) do
      [{{_guild_id, _user_id}, member}] ->
        {guild_id, member}

      [] ->
        :noop
    end
  end

  @doc "Bulk create a chunk of members for the given guild in the cache."
  @impl MemberCache
  def bulk_create(guild_id, members) do
    # CHONK like that one cat of craig
    new_members = Enum.map(members, &{{guild_id, &1.user.id}, Util.cast(&1, {:struct, Member})})
    true = :ets.insert(@table_name, new_members)
  end

  @doc "Get a QLC handle for the entire table."
  @doc since: "0.7.0"
  @impl MemberCache
  @spec qlc_handle :: :qlc.query_handle()
  def qlc_handle do
    unfold_matchspec = [{{{:"$1", :"$2"}, :"$3"}, [], [{{:"$1", :"$2", :"$3"}}]}]
    :ets.table(@table_name, traverse: {:select, unfold_matchspec})
  end
end
