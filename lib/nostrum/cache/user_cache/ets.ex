defmodule Nostrum.Cache.UserCache.ETS do
  @table_name :nostrum_users
  @moduledoc """
  An ETS-based cache for users.

  The default table name under which users are cached is `#{@table_name}`.
  In addition to the cache behaviour implementations provided by this module,
  you can also call regular ETS table methods on it, such as `:ets.info`.
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.UserCache

  alias Nostrum.Struct.User
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the ETS table name used for the cache."
  @spec tabname :: atom()
  def tabname, do: @table_name

  @doc "Bulk create a list of users from upstream data."
  @impl Nostrum.Cache.UserCache
  @spec bulk_create(Enum.t()) :: :ok
  def bulk_create(users) do
    Enum.each(users, &:ets.insert(@table_name, {&1.id, &1}))
  end

  @doc "Create a user from upstream data."
  @impl Nostrum.Cache.UserCache
  @spec create(map()) :: User.t()
  def create(user) do
    :ets.insert(@table_name, {user.id, user})
    User.to_struct(user)
  end

  @impl Nostrum.Cache.UserCache
  @spec delete(User.id()) :: User.t() | :noop
  def delete(id) do
    case lookup(id) do
      {:ok, user} ->
        :ets.delete(@table_name, id)
        User.to_struct(user)

      _ ->
        :noop
    end
  end

  @doc "Get a QLC handle for the backing table."
  @doc since: "0.7.0"
  @impl Nostrum.Cache.UserCache
  @spec qlc_handle :: :qlc.query_handle()
  def qlc_handle do
    :ets.table(@table_name)
  end

  @doc "Get a user by ID."
  @impl Nostrum.Cache.UserCache
  @spec get(User.id()) :: {:ok, User.t()} | {:error, atom}
  def get(id) do
    case lookup(id) do
      {:ok, user} -> {:ok, User.to_struct(user)}
      error -> error
    end
  end

  @doc "Update a user from upstream data."
  @impl Nostrum.Cache.UserCache
  @spec update(map()) :: {User.t(), User.t()} | :noop
  def update(info) do
    with {:ok, u} <- lookup(info.id),
         new_user = Map.merge(u, info),
         false <- u == new_user do
      :ets.insert(@table_name, {new_user.id, new_user})
      {User.to_struct(u), User.to_struct(new_user)}
    else
      {:error, _} ->
        # User just came online, make sure to cache if possible
        if Enum.all?([:username, :discriminator], &Map.has_key?(info, &1)),
          do: :ets.insert(@table_name, {info.id, info})

        :noop

      true ->
        :noop
    end
  end

  @doc false
  @spec lookup(User.id()) :: {:error, :user_not_found} | {:ok, map}
  def lookup(id) do
    case :ets.lookup(@table_name, id) do
      [] ->
        {:error, :user_not_found}

      [{^id, user}] ->
        {:ok, user}
    end
  end
end
