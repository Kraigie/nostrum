defmodule Nostrum.Cache.UserCache do
  @default_cache_implementation Nostrum.Cache.UserCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for users.

  You can call the functions provided by this module independent of which cache
  is configured, and it will dispatch to the configured cache implementation.

  By default, #{@default_cache_implementation} will be used for caching users.
  You can override this in the `:caches` option of the `:nostrum` application
  by setting the `:users` field to a different module implementing the behaviour
  defined by this module.

  See the documentation for the `Nostrum.Cache.GuildCache` module for more details.
  """

  alias Nostrum.Struct.User
  alias Nostrum.Util
  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @configured_cache :nostrum
                    |> Application.compile_env([:caches, :users], @default_cache_implementation)

  ## Supervisor callbacks

  @doc false
  defdelegate init(init_arg), to: @configured_cache

  @doc false
  defdelegate start_link(init_arg), to: @configured_cache

  @doc false
  defdelegate child_spec(opts), to: @configured_cache

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
  @callback create(payload :: map()) :: User.t()

  @doc ~S"""
  Bulk add multiple users to the cache at once.

  Returns `:ok`.
  """
  @callback bulk_create(user_payloads :: Enum.t()) :: :ok

  @doc ~S"""
  Update a user in the cache based on payload sent via the Gateway.

  Returns `:noop` if the user has not been updated in the cache, or
  `{old_user, new_user}` is the user has been written to the cache.
  """
  @callback update(payload :: map()) :: :noop | {User.t(), User.t()}

  @doc ~S"""
  Delete a user by ID.

  Returns the deleted user if present in the cache, or
  `:noop` if the user was not cached.
  """
  @callback delete(snowflake :: User.id()) :: :noop | User.t()

  @doc """
  Return a query handle for usage with `:qlc`.

  This is used by nostrum to provide automatic joins between the member and the
  user cache, and may be used for other functions in the future.

  The Erlang manual on [Implementing a QLC
  Table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table)
  contains examples for implementation.

  The query handle must return items in the form `{user_id, user}`, where
  `user_id` is a `t:Nostrum.Struct.User.id/0` and `user` is a
  `t:Nostrum.Struct.User.t/0`.
  """
  @doc since: "0.7.0"
  @callback qlc_handle() :: :qlc.query_handle()

  ## Dispatching
  defdelegate get(id), to: @configured_cache
  @doc false
  defdelegate create(payload), to: @configured_cache
  @doc false
  defdelegate bulk_create(users), to: @configured_cache
  @doc false
  defdelegate update(payload), to: @configured_cache
  @doc false
  defdelegate delete(snowflake), to: @configured_cache
  @doc false
  defdelegate qlc_handle(), to: @configured_cache

  @doc """
  Same as `c:get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @spec get!(User.id()) :: no_return | User.t()
  def get!(id) when is_snowflake(id), do: id |> get |> Util.bangify_find(id, __MODULE__)
end
