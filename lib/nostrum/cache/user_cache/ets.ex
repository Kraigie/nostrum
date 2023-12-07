defmodule Nostrum.Cache.UserCache.ETS do
  @moduledoc """
  An ETS-based cache for users.

  If you need to get the table reference for the table used by this module,
  please use the `table/0` function.
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.UserCache

  @table_name :nostrum_users

  alias Nostrum.Struct.User
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the ETS table."
  @impl Supervisor
  def init(_init_arg) do
    _tid = :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the ETS table reference used for the cache."
  @doc since: "0.8.0"
  @spec table :: :ets.table()
  def table, do: @table_name

  @doc "Bulk create a list of users from upstream data."
  @impl Nostrum.Cache.UserCache
  @spec bulk_create(Enum.t()) :: :ok
  def bulk_create(users) do
    Enum.each(users, &:ets.insert(@table_name, {&1.id, User.to_struct(&1)}))
  end

  @doc "Create a user from upstream data."
  @impl Nostrum.Cache.UserCache
  @spec create(map()) :: User.t()
  def create(payload) do
    parsed = User.to_struct(payload)
    :ets.insert(@table_name, {parsed.id, parsed})
    parsed
  end

  @impl Nostrum.Cache.UserCache
  @doc "Update a user from upstream data."
  @spec update(map()) :: {User.t() | nil, User.t()}
  def update(info) do
    converted = User.to_struct(info)

    with {:ok, old_user} <- lookup(info.id),
         new_user = Map.merge(old_user, info),
         false <- old_user == new_user do
      :ets.insert(@table_name, {new_user.id, new_user})
      {old_user, new_user}
    else
      {:error, _} ->
        # User just came online, make sure to cache if possible
        # TODO: check for `:global_name` once fully rolled out?
        if Enum.all?([:username, :discriminator], &Map.has_key?(info, &1)),
          do: :ets.insert(@table_name, {info.id, info})

        {nil, converted}

      true ->
        {nil, converted}
    end
  end

  @doc false
  @spec lookup(User.id()) :: {:error, :user_not_found} | {:ok, map}
  defp lookup(id) do
    case :ets.lookup(@table_name, id) do
      [] ->
        {:error, :user_not_found}

      [{^id, user}] ->
        {:ok, user}
    end
  end

  @impl Nostrum.Cache.UserCache
  @spec delete(User.id()) :: User.t() | :noop
  def delete(id) do
    case lookup(id) do
      {:ok, user} ->
        :ets.delete(@table_name, id)
        user

      _ ->
        :noop
    end
  end

  @doc "Get a QLC query handle for the user cache."
  @doc since: "0.7.0"
  @impl Nostrum.Cache.UserCache
  @spec query_handle :: :qlc.query_handle()
  def query_handle do
    :ets.table(@table_name)
  end
end
