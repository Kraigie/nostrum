defmodule Nostrum.Cache.UserCache do
  @default_cache_implementation Nostrum.Cache.UserCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for users.
  """

  alias Nostrum.Struct.User
  alias Nostrum.Util
  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @configured_cache :nostrum
                    |> Application.get_env(:caches, %{})
                    |> Map.get(:users, @default_cache_implementation)

  ## Behaviour specification

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
  @callback get(id :: User.id()) :: {:ok, User.t()} | {:error, atom}

  @doc ~S"""
  Add a new user to the cache based on the Discord Gateway payload.

  Returns a `t:Nostrum.Struct.User.t/0` struct representing the created user.
  """
  @callback create(payload :: Map.t()) :: User.t()

  @doc ~S"""
  Bulk add multiple users to the cache at once.

  Returns `:ok`.
  """
  @callback bulk_create(user_payloads :: [Map.t()]) :: :ok

  @doc ~S"""
  Update a user in the cache based on payload sent via the Gateway.

  Returns `:noop` if the user has been updated in the cache, or
  `{old_user, new_user}` is the user has been written to the cache.
  """
  @callback update(payload :: Map.t()) :: :noop | {User.t(), User.t()}

  @doc ~S"""
  Delete a user by ID.

  Returns the deleted user if present in the cache, or
  `:noop` if the user was not cached.
  """
  @callback delete(snowflake :: User.id()) :: :noop | User.t()

  ## Dispatching

  @doc "Retrieve a user using the selected cache implementation."
  @spec get(User.id()) :: {:error, atom} | {:ok, User.t()}
  def get(id) when is_snowflake(id) do
    @configured_cache.get(id)
  end

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @spec get!(User.id()) :: no_return | User.t()
  def get!(id) when is_snowflake(id), do: id |> get |> Util.bangify_find(id, __MODULE__)

  @doc "Create a user using the selected cache implementation."
  @spec create(Map.t()) :: User.t()
  def create(payload) do
    @configured_cache.create(payload)
  end

  @doc "Bulk create multiple users using the selected cache implementation."
  @spec bulk_create([Map.t()]) :: :ok
  def bulk_create(users) do
    @configured_cache.bulk_create(users)
  end

  @doc "Update the given user using the selected cache implementation."
  @spec update(Map.t()) :: :noop | {User.t(), User.t()}
  def update(payload) do
    @configured_cache.update(payload)
  end

  @doc "Delete a user by ID using the selected cache implementation."
  @spec delete(User.id()) :: :noop | User.t()
  def delete(id) when is_snowflake(id) do
    @configured_cache.delete(id)
  end
end
