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
  alias Nostrum.Snowflake
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
  Wrap long-running queries operations.

  ## Safety {: .note}

  Any operations are surrounded by `:ets.safe_fixtable`. It is therefore
  recommended to finish your read quickly.
  """
  @doc since: "0.10.0"
  @impl MemberCache
  @spec wrap_query((-> query_result)) :: query_result when query_result: term()
  def wrap_query(fun) do
    :ets.safe_fixtable(@table_name, true)
    fun.()
  after
    :ets.safe_fixtable(@table_name, false)
  end

  # Used by dispatch

  @impl MemberCache
  @doc "Retrieve the member for the given guild and user in the cache."
  @spec get(Guild.id(), Member.user_id()) :: {:ok, Member.t()} | {:error, any()}
  def get(guild_id, user_id) do
    case :ets.lookup(@table_name, {guild_id, user_id}) do
      [{_, member}] ->
        {:ok, member}

      [] ->
        {:error, :member_not_found}
    end
  end

  @impl MemberCache
  @doc since: "0.10.0"
  def by_user(user_id) do
    ms = [{{{:"$1", user_id}, :"$2"}, [], [{{:"$1", :"$2"}}]}]

    Stream.resource(
      fn -> :ets.select(@table_name, ms, 100) end,
      fn items ->
        case items do
          {matches, cont} ->
            {matches, :ets.select(cont)}

          :"$end_of_table" ->
            {:halt, nil}
        end
      end,
      fn _cont -> :ok end
    )
  end

  @impl MemberCache
  @doc since: "0.10.0"
  def by_guild(guild_id) do
    ms = [{{{guild_id, :_}, :"$1"}, [], [:"$1"]}]

    Stream.resource(
      fn -> :ets.select(@table_name, ms, 100) end,
      fn items ->
        case items do
          {matches, cont} ->
            {matches, :ets.select(cont)}

          :"$end_of_table" ->
            {:halt, nil}
        end
      end,
      fn _cont -> :ok end
    )
  end

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
    # Force keys to be atoms before casting just to simplify finding the user_id
    # because of the atom/string ambiguity issues from the gateway that Discord
    # won't fix.

    member_payload = Map.new(payload, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    member_id = Util.cast(member_payload[:user][:id], Snowflake)

    case :ets.lookup(@table_name, {guild_id, member_id}) do
      [{key, old_member}] ->
        new_member = Member.to_struct(member_payload, old_member)
        true = :ets.update_element(@table_name, key, {2, new_member})
        {guild_id, old_member, new_member}

      [] ->
        new_member = Util.cast(member_payload, {:struct, Member})
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
end
