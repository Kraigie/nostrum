defmodule Nostrum.Cache.PresenceCache do
  @default_cache_implementation Nostrum.Cache.PresenceCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for Discord presences.

  By default, `#{@default_cache_implementation}` will be use for caching
  presences.  You can override this in the `:caches` option of the `nostrum`
  application by setting the `:presences` fields to a different module
  implementing the `Nostrum.Cache.PresenceCache` behaviour. Any module below
  `Nostrum.Cache.PresenceCache` implements this behaviour and can be used as a
  cache.

  ## Writing your own presence cache

  As with the other caches, the presence cache API consists of two parts:

  - The functions that nostrum calls, such as `c:create/1` or `c:update/1`.
  These **do not create any objects in the Discord API**, they are purely
  created to update the cached data from data that Discord sends us. If you
  want to create objects on Discord, use the functions exposed by `Nostrum.Api`
  instead.

  - the QLC query handle for read operations, `c:query_handle/0`, and

  - the `c:child_spec/1` callback for starting the cache under a supervisor.

  You need to implement both of them for nostrum to work with your custom
  cache.
  """

  @moduledoc since: "0.5.0"

  @configured_cache Nostrum.Cache.Base.get_cache_module(:presences, @default_cache_implementation)

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.Util
  import Nostrum.Snowflake, only: [is_snowflake: 1]

  # Types
  @typedoc """
  Represents a presence as received from Discord.
  See [Presence Update](https://discord.com/developers/docs/topics/gateway#presence-update).
  """
  @typedoc since: "0.5.0"
  @opaque presence :: map()

  # Callbacks
  @doc ~S"""
  Retrieves a presence for a user from the cache by guild and id.

  If successful, returns `{:ok, presence}`. Otherwise returns `{:error, reason}`.

  ## Example
  ```elixir
  case Nostrum.Cache.PresenceCache.get(111133335555, 222244446666) do
    {:ok, presence} ->
      "They're #{presence.status}"
    {:error, _reason} ->
      "They're dead Jim"
  end
  ```
  """
  @spec get(Guild.id(), User.id()) :: {:ok, presence()} | {:error, any()}
  defdelegate get(guild_id, user_id), to: @configured_cache

  @callback get(Guild.id(), User.id()) :: {:ok, presence()} | {:error, any()}

  @doc """
  Create a presence in the cache.
  """
  @callback create(presence) :: :ok

  @doc """
  Bulk create multiple presences for the given guild in the cache.
  """
  @callback bulk_create(Guild.id(), [presence()]) :: :ok

  @doc """
  Update the given presence in the cache from upstream data.

  ## Return value

  Return the guild ID along with the old presence (if it was cached, otherwise
  `nil`) and the updated presence structure. If the `:activities` or `:status`
  fields of the presence did not change, return `:noop`.
  """
  @callback update(map()) ::
              {Guild.id(), old_presence :: presence() | nil, new_presence :: presence()} | :noop

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  A function that should wrap any `:qlc` operations.

  If you implement a cache that is backed by a database and want to perform
  cleanup and teardown actions such as opening and closing connections,
  managing transactions and so on, you want to implement this function. nostrum
  will then effectively call `wrap_query(fn -> ... end)`.

  If your cache does not need any wrapping, you can omit this.
  """
  @doc since: "0.8.0"
  @callback wrap_query((-> result)) :: result when result: term()
  @optional_callbacks wrap_query: 1

  # Dispatch
  @doc false
  defdelegate create(presence), to: @configured_cache
  @doc false
  defdelegate update(presence), to: @configured_cache
  @doc false
  defdelegate bulk_create(guild_id, presences), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

  # Dispatch helpers
  @doc "Same as `get/1`, but raise `Nostrum.Error.CacheError` in case of a failure."
  @spec get!(User.id(), Guild.id()) :: presence() | no_return()
  @spec get!(User.id(), Guild.id(), module()) :: presence() | no_return()
  def get!(guild_id, user_id, cache \\ @configured_cache)
      when is_snowflake(user_id) and is_snowflake(guild_id) do
    guild_id
    |> get(user_id)
    |> Util.bangify_find({guild_id, user_id}, cache)
  end

  @doc """
  Call `c:wrap_query/1` on the given cache, if implemented.

  If no cache is given, calls out to the default cache.
  """
  @doc since: "0.8.0"
  @spec wrap_query((-> result)) :: result when result: term()
  @spec wrap_query(module(), (-> result)) :: result when result: term()
  def wrap_query(cache \\ @configured_cache, fun) do
    if function_exported?(cache, :wrap_query, 1) do
      cache.wrap_query(fun)
    else
      fun.()
    end
  end
end
