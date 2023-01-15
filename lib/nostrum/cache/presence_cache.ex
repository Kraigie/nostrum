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

  - The functions that the user calls, currently only `c:get/2`.

  - The functions that nostrum calls, such as `c:create/1` or `c:update/1`.
  These **do not create any objects in the Discord API**, they are purely
  created to update the cached data from data that Discord sends us. If you
  want to create objects on Discord, use the functions exposed by `Nostrum.Api`
  instead.

  You need to implement both of them for nostrum to work with your custom
  cache. **You also need to implement `Supervisor` callbacks**, which will
  start your cache as a child under `Nostrum.Cache.CacheSupervisor`: As an
  example, the `Nostrum.Cache.PresenceCache.ETS` implementation uses this to to
  set up its ETS table it uses for caching. See the callbacks section for every
  nostrum-related callback you need to implement.
  """

  @moduledoc since: "0.5.0"

  @configured_cache :nostrum
                    |> Application.compile_env(
                      [:caches, :presences],
                      @default_cache_implementation
                    )

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.Util
  import Nostrum.Snowflake, only: [is_snowflake: 1]

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate init(init_arg), to: @configured_cache
  @doc false
  defdelegate start_link(init_arg), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

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
  @callback get(User.id(), Guild.id()) :: {:ok, presence()} | {:error, :presence_not_found}

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

  # Dispatch
  @doc section: :reading
  defdelegate get(user_id, guild_id), to: @configured_cache
  @doc false
  defdelegate create(presence), to: @configured_cache
  @doc false
  defdelegate update(presence), to: @configured_cache
  @doc false
  defdelegate bulk_create(guild_id, presences), to: @configured_cache

  # Dispatch helpers
  @doc "Same as `get/1`, but raise `Nostrum.Error.CacheError` in case of a failure."
  @doc section: :reading
  @spec get!(User.id(), Guild.id()) :: presence() | no_return()
  def get!(user_id, guild_id) when is_snowflake(user_id) and is_snowflake(guild_id) do
    user_id
    |> @configured_cache.get(guild_id)
    |> Util.bangify_find({user_id, guild_id}, @configured_cache)
  end
end
