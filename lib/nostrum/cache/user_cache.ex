defmodule Nostrum.Cache.UserCache do
  @moduledoc """
  Cache for users.

  The ETS table name associated with the User Cache is `:users`. Besides the
  methods provided below you can call any other ETS methods on the table.

  ## Example
  ```elixir
  info = :ets.info(:users)
  [..., heir: :none, name: :users, size: 1, ...]
  size = info[:size]
  1
  ```
  """

  alias Nostrum.Struct.User
  alias Nostrum.Util

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~s"""
  Retrieves a user from the cache by id.

  If successful, returns `{:ok, user}`. Otherwise, returns `{:error, reason}`.

  ## Example
  ```elixir
  case Nostrum.Cache.UserCache.get(1111222233334444) do
    {:ok, user} ->
      "We found " <> user.username
    {:error, _reason} ->
      "No es bueno"
  end
  ```
  """
  @spec get(User.id()) :: {:error, atom} | {:ok, User.t()}
  def get(id) when is_snowflake(id) do
    case lookup(id) do
      {:ok, user} -> {:ok, User.to_struct(user)}
      error -> error
    end
  end

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @spec get!(User.id()) :: no_return | User.t()
  def get!(id) when is_snowflake(id), do: id |> get |> Util.bangify_find(id, __MODULE__)

  @doc false
  @spec create(map) :: User.t()
  def create(user) do
    :ets.insert(:users, {user["id"], user})
    User.to_struct(user)
  end

  @doc false
  @spec create([map]) :: :ok
  def bulk_create(members) do
    Enum.each(members, &:ets.insert(:users, {&1.user.id, &1.user}))
  end

  @doc false
  @spec update(map) :: :noop | {User.t(), User.t()}
  def update(info) do
    with {:ok, u} <- lookup(info.id),
         new_user = Map.merge(u, info),
         false <- u == new_user do
      :ets.insert(:users, {new_user.id, new_user})
      {User.to_struct(u), User.to_struct(new_user)}
    else
      {:error, _} ->
        # User just came online, make sure to cache if possible
        if Enum.all?([:username, :discriminator], &Map.has_key?(info, &1)),
          do: :ets.insert(:users, {info.id, info})

        :noop

      true ->
        :noop
    end
  end

  @doc false
  @spec delete(User.id()) :: :noop | User.t()
  def delete(id) do
    case lookup(id) do
      {:ok, user} ->
        :ets.delete(:users, id)
        User.to_struct(user)

      _ ->
        :noop
    end
  end

  @doc false
  @spec lookup(User.id()) :: {:error, :user_not_found} | {:ok, map}
  def lookup(id) do
    case :ets.lookup(:users, id) do
      [] ->
        {:error, :user_not_found}

      [{^id, user}] ->
        {:ok, user}
    end
  end
end
