defmodule Mixcord.Cache.UserCache do
  @moduledoc """
  Cache for users.

  The ETS table name associated with the User Cache is `:users`. Besides the
  methods provided below you can call any other ETS methods on the table.

  **Example**
  ```elixir
  info = :ets.info(:users)
  [..., heir: :none, name: :users, size: 1, ...]
  size = info[:size]
  1
  ```
  """

  use GenServer

  alias Mixcord.Struct.User
  alias Mixcord.Util

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: UserCache)
  end

  @doc false
  def init(state) do
    {:ok, state}
  end

  @doc ~s"""
  Retrieves a user from the cache.

  `get/1` requires a keyword list as its only argument.

  Returns {:ok, Mixcord.Struct.User.t} if found, {:error, atom} otherwise.

  **Example**
  ```elixir
  case Mixcord.Cache.User.get(id: 1111222233334444) do
    {:ok, user} ->
      "We found " <> user.username
    {:error, _reason} ->
      "No es bueno"
  end
  ```
  """
  @spec get(id: integer) :: {:error, atom} | {:ok, Mixcord.Struct.User.t}
  def get(id: id), do: lookup_as_struct(id)

  @doc """
  Retrieves a user from the cache.

  See `get/1` for use and examples.

  Returns `Mixcord.Struct.User.t` if found.
  Raises `Mixcord.Error.CahceError` if not found.
  """
  @spec get!(id: integer) :: no_return | Mixcord.Struct.User.t
  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find(id, __MODULE__)
  end

  @doc false
  def create(user) do
    case GenServer.call(UserCache, {:create, user.id, user}) do
      {res} ->
        {:ok, {res}}
      error ->
        error
    end
  end

  @doc false
  def update(user) do
    case GenServer.call(UserCache, {:update, user.id, user}) do
      {res} ->
        {:ok, {res}}
      error ->
        error
    end
  end

  @doc false
  def delete(user) do
    case GenServer.call(UserCache, {:delete, user.id}) do
      {res} ->
        {:ok, {res}}
      error ->
        error
    end
  end

  def handle_call({:create, id, user}, _from, state) do
    :ets.insert(:users, insert(id, user))
    {:reply, {User.to_struct(user)}, state}
  end

  def handle_call({:update, id, user}, _from, state) do
    case :ets.lookup(:users, {:id, id}) do
      [] ->
        {:error, :user_not_found}
      [lookup] ->
        :ets.insert(:users, insert(id, user))
        {:reply, {lookup_to_struct(lookup), User.to_struct(user)}, state}
    end
  end

  def handle_call({:delete, id}, _from, state) do
    case :ets.lookup(:users, {:id, id}) do
      [] ->
        {:error, :user_not_found}
      [lookup] ->
        :ets.delete(:users, {:id, id})
        {:reply, {lookup_to_struct(lookup)}, state}
    end
  end

  @doc false
  def insert(id, map) do
    map
    |> remove_struct_key
    |> Map.to_list
    |> List.insert_at(0, {:id, id})
    |> List.to_tuple
  end

  def remove_struct_key(%{__struct__: _} = map), do: Map.delete(map, :__struct__)
  def remove_struct_key(map), do: map

  @doc false
  def lookup_to_struct(map) do
    map |> Tuple.to_list |> Enum.into(%{}) |> User.to_struct
  end

  @doc false
  def lookup_as_struct(id) do
    case :ets.lookup(:users, {:id, id}) do
      [] ->
        {:error, :user_not_found}
      [other] ->
        lookup =
          other
          |> Tuple.to_list
          |> Enum.into(%{})
          |> User.to_struct
        {:ok, lookup}
    end
  end

end
