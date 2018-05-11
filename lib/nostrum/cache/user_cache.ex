defmodule Nostrum.Cache.UserCache do
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

  alias Nostrum.Struct.User
  alias Nostrum.Util

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: UserCache)
  end

  @doc false
  def init([]) do
    {:ok, []}
  end

  @doc ~s"""
  Retrieves a user from the cache.

  `get/1` requires a keyword list as its only argument.

  Returns {:ok, Nostrum.Struct.User.t} if found, {:error, atom} otherwise.

  **Example**
  ```elixir
  case Nostrum.Cache.UserCache.get(id: 1111222233334444) do
    {:ok, user} ->
      "We found " <> user.username
    {:error, _reason} ->
      "No es bueno"
  end
  ```
  """
  @spec get(id: integer) :: {:error, atom} | {:ok, Nostrum.Struct.User.t()}
  def get(id: id), do: lookup_as_struct(id)

  @doc """
  Retrieves a user from the cache.

  See `get/1` for use and examples.

  Returns `Nostrum.Struct.User.t` if found.
  Raises `Nostrum.Error.CahceError` if not found.
  """
  @spec get!(id: integer) :: no_return | Nostrum.Struct.User.t()
  def get!(id: id) do
    get(id: id)
    |> Util.bangify_find(id, __MODULE__)
  end

  @doc false
  def create(user) do
    GenServer.call(UserCache, {:create, user.id, user})
  end

  @doc false
  def update(user) do
    GenServer.call(UserCache, {:update, user.id, user})
  end

  @doc false
  def delete(user) do
    GenServer.call(UserCache, {:delete, user.id})
  end

  def handle_call({:create, id, %{bot: _} = user}, _from, state) do
    :ets.insert(:users, insert(id, user))
    {:reply, User.to_struct(user), state}
  end

  def handle_call({:create, id, user}, _from, state) do
    # We don't always get the `bot` key, so we'll force it in here.
    # This allows us to lookup ets table by element as they're all guaranteed to
    # be there.
    # REVIEW: While, arbitrary, this looks to be deterministic.
    # Relevant docs: http://erlang.org/doc/man/maps.html#to_list-1
    :ets.insert(:users, insert(id, Map.put(user, :bot, false)))
    {:reply, User.to_struct(user), state}
  end

  def handle_call({:update, id, user}, _from, state) do
    case :ets.lookup(:users, {:id, id}) do
      [] ->
        {:reply, {:error, :user_not_found}, state}

      [lookup] ->
        :ets.insert(:users, insert(id, user))
        {:reply, {lookup_to_struct(lookup), User.to_struct(user)}, state}
    end
  end

  def handle_call({:delete, id}, _from, state) do
    case :ets.lookup(:users, {:id, id}) do
      [] ->
        {:reply, {:error, :user_not_found}, state}

      [lookup] ->
        :ets.delete(:users, {:id, id})
        {:reply, lookup_to_struct(lookup), state}
    end
  end

  @doc false
  def insert(id, map) do
    map
    |> remove_struct_key
    |> Map.to_list()
    # We'll have id key twice; Isn't an issue and allows us to have `id` as key.
    |> List.insert_at(0, {:id, id})
    |> List.to_tuple()
  end

  def remove_struct_key(%{__struct__: _} = map), do: Map.delete(map, :__struct__)
  def remove_struct_key(map), do: map

  @doc false
  def lookup_to_struct(map) do
    map |> Tuple.to_list() |> Enum.into(%{}) |> User.to_struct()
  end

  @doc false
  def lookup_as_struct(id) do
    case :ets.lookup(:users, {:id, id}) do
      [] ->
        {:error, :user_not_found}

      [other] ->
        lookup = lookup_to_struct(other)
        {:ok, lookup}
    end
  end
end
