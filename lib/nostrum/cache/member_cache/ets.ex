defmodule Nostrum.Cache.MemberCache.ETS do
  @moduledoc """
  An ETS-based cache for members.

  If you need to get the table reference for the table used by this module,
  please use the `table/0` function.
  """
  @moduledoc since: "0.7.0"

  @behaviour Nostrum.Cache.MemberCache

  @table_name :nostrum_guild_members

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
    _tid = :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the ETS table reference used for the cache."
  @doc since: "0.8.0"
  @spec table :: :ets.table()
  def table, do: @table_name

  @doc "Clear any objects in the cache."
  @doc since: "0.8.0"
  @spec clear() :: :ok
  def clear do
    :ets.delete_all_objects(@table_name)
    :ok
  end

  @doc """
  Wrap QLC operations.

  ## Safety {: .note}

  Any QLC operations are surrounded by `:ets.safe_fixtable`. It is therefore
  recommended to finish your read quickly.
  """
  @doc since: "0.8.0"
  @impl MemberCache
  @spec wrap_qlc((-> qlc_result)) :: qlc_result when qlc_result: term()
  def wrap_qlc(fun) do
    :ets.safe_fixtable(@table_name, true)
    fun.()
  after
    :ets.safe_fixtable(@table_name, false)
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

  @impl MemberCache
  @doc "Remove the given member from the given guild in the cache."
  @spec delete(Guild.id(), Member.user_id()) :: {Guild.id(), Member.t()} | :noop
  def delete(guild_id, user_id) do
    case :ets.take(@table_name, {guild_id, user_id}) do
      [{{_guild_id, _user_id}, member}] ->
        {guild_id, member}

      [] ->
        :noop
    end
  end

  @impl MemberCache
  @doc "Bulk create a chunk of members for the given guild in the cache."
  def bulk_create(guild_id, members) do
    # CHONK like that one cat of craig
    new_members = Enum.map(members, &{{guild_id, &1.user.id}, Util.cast(&1, {:struct, Member})})
    true = :ets.insert(@table_name, new_members)
  end

  @impl MemberCache
  @doc "Get a QLC query handle for the member cache."
  @doc since: "0.7.0"
  @spec query_handle :: :qlc.query_handle()
  def query_handle do
    :ets.table(@table_name)
  end
end
