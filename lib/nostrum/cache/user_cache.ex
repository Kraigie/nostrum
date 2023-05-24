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

  ## Behaviour specification

  @doc ~s"""
  Retrieves a user from the cache by id.

  This function can be called with the cache to use as an optional argument. By
  default, the cache configured at compile time is used.

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
  @spec get(User.id()) :: {:ok, User.t()} | {:error, atom}
  @spec get(User.id(), module()) :: {:ok, User.t()} | {:error, atom}
  def get(user_id, cache \\ @configured_cache) do
    handle = :nostrum_user_cache_qlc.get(user_id, cache)

    wrap_qlc(cache, fn ->
      case :qlc.eval(handle) do
        [{_user_id, user}] ->
          {:ok, user}

        [] ->
          {:error, :not_found}
      end
    end)
  end

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
  @callback update(payload :: map()) :: {User.t() | nil, User.t()}

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
  @callback query_handle() :: :qlc.query_handle()

  @doc """
  A function that should wrap any `:qlc` operations.

  If you implement a cache that is backed by a database and want to perform
  cleanup and teardown actions such as opening and closing connections,
  managing transactions and so on, you want to implement this function. Nostrum
  will then effectively call `wrap_qlc(fn -> :qlc.e(...) end)`.

  If your cache does not need any wrapping, you can omit this.
  """
  @doc since: "0.8.0"
  @callback wrap_qlc((() -> result)) :: result when result: term()
  @optional_callbacks wrap_qlc: 1

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @spec get!(User.id()) :: no_return | User.t()
  def get!(id) when is_snowflake(id), do: id |> get |> Util.bangify_find(id, __MODULE__)

  @doc "Call `c:query_handle/0` on the configured cache."
  @doc since: "0.8.0"
  @spec query_handle() :: :qlc.query_handle()
  defdelegate query_handle, to: @configured_cache

  @doc """
  Call `c:wrap_qlc/1` on the given cache, if implemented.

  If no cache is given, calls out to the default cache.
  """
  @doc since: "0.8.0"
  @spec wrap_qlc((() -> result)) :: result when result: term()
  @spec wrap_qlc(module(), (() -> result)) :: result when result: term()
  def wrap_qlc(cache \\ @configured_cache, fun) do
    if function_exported?(cache, :wrap_qlc, 1) do
      cache.wrap_qlc(fun)
    else
      fun.()
    end
  end

  # The Innards

  ## Dispatching
  @doc false
  defdelegate create(payload), to: @configured_cache
  @doc false
  defdelegate bulk_create(users), to: @configured_cache
  @doc false
  defdelegate update(payload), to: @configured_cache
  @doc false
  defdelegate delete(snowflake), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
