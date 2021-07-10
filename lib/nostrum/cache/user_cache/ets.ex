defmodule Nostrum.Cache.UserCache.ETS do
  @table_name :users
  @moduledoc """
  An ETS-based cache for users.

  The default table name under which users are cached is `#{@table_name}`.
  In addition to the cache behaviour implementations provided by this module,
  you can also call regular ETS table methods on it, such as `:ets.info`.
  """

  @behaviour Nostrum.Cache.UserCache

  alias Nostrum.Struct.User

  @doc "Set up the cache for tests."
  def setup do
    :ets.new(@table_name, [:set, :public, :named_table])
  end

  @impl Nostrum.Cache.UserCache
  @spec bulk_create([Map.t()]) :: :ok
  def bulk_create(members) do
    Enum.each(members, &:ets.insert(@table_name, {&1.user.id, &1.user}))
  end

  @impl Nostrum.Cache.UserCache
  @spec create(Map.t()) :: User.t()
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

  @impl Nostrum.Cache.UserCache
  @spec get(User.id()) :: {:ok, User.t()} | {:error, atom}
  def get(id) do
    case lookup(id) do
      {:ok, user} -> {:ok, User.to_struct(user)}
      error -> error
    end
  end

  @impl Nostrum.Cache.UserCache
  @spec update(Map.t()) :: {User.t(), User.t()} | :noop
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
