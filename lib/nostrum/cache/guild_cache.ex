defmodule Nostrum.Cache.GuildCache do
  @default_cache_implementation Nostrum.Cache.GuildCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for guilds.

  You can call the functions provided by this module independent of which cache
  is configured, and it will dispatch to the configured cache implementation.

  By default, #{@default_cache_implementation} will be used for caching guilds.
  You can override this in the `:caches` option of the `:nostrum` application
  by setting the `:guilds` field to a different module implementing the
  `Nostrum.Cache.GuildCache` behaviour. Any module below
  `Nostrum.Cache.GuildCache` can be used as a cache.

  ## Writing your own guild cache

  As with the other caches, the guild cache API consists of two parts:

  - The functions that nostrum calls, such as `c:create/1` or `c:update/1`.
  These **do not create any objects in the Discord API**, they are purely
  created to update the cached data from data that Discord sends us. If you
  want to create objects on Discord, use the functions exposed by `Nostrum.Api`
  instead.

  - the QLC query handle for read operations, `c:query_handle/0`, and

  - the `c:child_spec/1` callback for starting the cache under a supervisor.

  You need to implement all of them for nostrum to work with your custom
  cache. 

  The "upstream data" wording in this module references the fact that the
  data that the guild cache (and other caches) retrieves represents the raw
  data we receive from the upstream connection, no attempt is made by nostrum
  to sanitize the data before it enters the cache. Caching implementations
  need to cast the data to the resulting type themselves. A possible future
  improvement would be moving the data casting into this module before the
  backing cache implementation is called.
  """

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Util

  @configured_cache :nostrum
                    |> Application.compile_env([:caches, :guilds], @default_cache_implementation)

  @typedoc "A selector for looking up entries in the cache."
  @type selector :: (Guild.t() -> any)

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

  ## Behaviour specification

  @doc """
  Retrieves all `Nostrum.Struct.Guild` from the cache.
  """
  @doc deprecated: "Use fold/2-3 instead"
  @deprecated "Use fold/2-3 instead"
  def all(cache \\ @configured_cache) do
    wrap_qlc(cache, fn ->
      :qlc.e(:nostrum_guild_cache_qlc.all(cache))
    end)
  end

  @doc """
  Fold (reduce) over all guilds in the cache.

  ## Parameters

  - `acc`: The initial accumulator. Also returned if no guilds are cached.
  - `fun`: Called for every guild in the result. Takes a pair in the form
  `{guild, acc}`, and must return the updated accumulator.
  - `cache` (optional): The cache to use. nostrum will use the cache configured
  at compile time by default.
  """
  @doc since: "0.8.0"
  @spec fold(acc, (Guild.t(), acc -> acc)) :: acc when acc: term()
  @spec fold(acc, (Guild.t(), acc -> acc), module()) :: acc when acc: term()
  def fold(acc, reducer, cache \\ @configured_cache) do
    handle = :nostrum_guild_cache_qlc.all(cache)
    wrap_qlc(cache, fn -> :qlc.fold(reducer, acc, handle) end)
  end

  @doc """
  Selects guilds matching `selector` from all `Nostrum.Struct.Guild` in the cache.
  """
  @doc deprecated: "Use fold/2-3 instead"
  @deprecated "Use fold/2-3 instead"
  @spec select_all(selector :: (Guild.t() -> any())) :: Enum.t()
  def select_all(selector) when is_function(selector, 1) do
    handle = :nostrum_guild_cache_qlc.all(@configured_cache)
    folder = fn {_id, guild}, acc -> [selector.(guild) | acc] end
    wrap_qlc(@configured_cache, fn -> :qlc.fold(folder, [], handle) end)
  end

  @doc """
  Retrieves a single `Nostrum.Struct.Guild` from the cache via its `id`.

  Returns `{:error, :not_found}` if no result was found.
  """
  @spec get(Guild.id()) :: {:ok, Guild.t()} | {:error, :not_found}
  @spec get(Guild.id(), module()) :: {:ok, Guild.t()} | {:error, :not_found}
  def get(guild_id, cache \\ @configured_cache) do
    handle = :nostrum_guild_cache_qlc.get(guild_id, cache)

    wrap_qlc(cache, fn ->
      case :qlc.eval(handle) do
        [guild] -> {:ok, guild}
        [] -> {:error, :not_found}
      end
    end)
  end

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild`.

  Returns `{:error, reason}` if no result was found.
  """
  @doc deprecated:
         "Use `with {:ok, result} = GuildCache.get(guild_id), my_result = selector(result)` instead"
  @deprecated "Use `with {:ok, result} = GuildCache.get(guild_id), my_result = selector(result)` instead"
  @spec select(Guild.id(), selector) :: {:ok, any} | {:error, :not_found}
  def select(guild_id, selector) do
    case get(guild_id) do
      {:ok, guild} ->
        {:ok, selector.(guild)}

      error ->
        error
    end
  end

  # Functions called from nostrum.

  @doc "Create a guild in the cache."
  @callback create(map()) :: Guild.t()

  @doc """
  Update a guild from upstream data.

  Return the original guild before the update (if it was cached) and the
  updated guild.
  """
  @callback update(map()) :: {old_guild :: Guild.t() | nil, updated_guild :: Guild.t()}

  @doc """
  Delete a guild from the cache.

  Return the old guild if it was cached, or `nil` otherwise.
  """
  @callback delete(Guild.id()) :: Guild.t() | nil

  @doc """
  Create a channel for the guild from upstream data.

  Return the adapted `t:Nostrum.Struct.Channel.t/0` structure.
  """
  @callback channel_create(Guild.id(), channel :: map()) :: Channel.t()

  @doc """
  Delete the given channel from the guild.

  If the channel was cached, return the original channel. Return `:noop`
  otherwise.
  """
  @callback channel_delete(Guild.id(), Channel.id()) :: Channel.t() | :noop

  @doc """
  Update the given channel on the given guild from upstream data.

  Return the original channel before the update if known, and the updated
  channel.
  """
  @callback channel_update(Guild.id(), channel :: map()) ::
              {old_channel :: Channel.t() | nil, new_channel :: Channel.t()}

  @doc """
  Update the emoji list of the given guild from upstream data.

  Discord sends us the complete emoji list on an update, which is passed here.

  Return the old list of emojis before the update, and the updated list of
  emojis.
  """
  @callback emoji_update(Guild.id(), emojis :: [map()]) ::
              {old_emojis :: [Emoji.t()], new_emojis :: [Emoji.t()]}

  @doc """
  Create a role on the given guild from upstream data.

  Return the casted role.
  """
  @callback role_create(Guild.id(), role :: map()) :: {Guild.id(), new_role :: Role.t()}

  @doc """
  Delete the given role on the given guild.

  Return the guild and the old role if it was cached, or `:noop` otherwise.
  """
  @callback role_delete(Guild.id(), Role.id()) :: {Guild.id(), old_role :: Role.t()} | :noop

  @doc """
  Update a role on the given guild from upstream data.

  Return the old role before the update and the updated role.
  """
  @callback role_update(Guild.id(), role :: map()) ::
              {Guild.id(), old_role :: Role.t() | nil, new_role :: Role.t()}

  @doc """
  Update the voice state of the given guild from upstream data.

  Note that it is recommended to drop the `:member` / `"member"` keys of
  the supplied upstream data, as these would otherwise duplicate the data
  that is being kept in the guild cache already.

  Return the guild ID and the updated voice states of the guild.
  """
  @callback voice_state_update(Guild.id(), state :: map()) :: {Guild.id(), new_state :: [map()]}

  @doc """
  Increment the member count for this guild by one.
  """
  @doc since: "0.7.0"
  @callback member_count_up(Guild.id()) :: true

  @doc """
  Decrement the member count for this guild by one.
  """
  @doc since: "0.7.0"
  @callback member_count_down(Guild.id()) :: true

  @doc """
  Return a QLC query handle for cache read operations.

  This is used by nostrum to provide any read operations on the cache. Write
  operations still need to be implemented separately.

  The Erlang manual on [Implementing a QLC
  Table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table)
  contains examples for implementation. To prevent full table scans, accept
  match specifications in your `TraverseFun` and implement a `LookupFun` as
  documented.

  The query handle must return items in the form `{guild_id, guild}`, where:
  - `guild_id` is a `t:Nostrum.Struct.Guild.id/0`, and
  - `guild` is a `t:Nostrum.Struct.Guild.t/0`.

  If your cache needs some form of setup or teardown for QLC queries (such as
  opening connections), see `c:wrap_qlc/1`.
  """
  @doc since: "0.8.0"
  @callback query_handle() :: :qlc.query_handle()

  @doc """
  A function that should wrap any `:qlc` operations.

  If you implement a cache that is backed by a database and want to perform
  cleanup and teardown actions such as opening and closing connections,
  managing transactions and so on, you want to implement this function. nostrum
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

  # Dispatching logic.
  @doc false
  defdelegate create(guild), to: @configured_cache
  @doc false
  defdelegate update(guild), to: @configured_cache
  @doc false
  defdelegate delete(guild_id), to: @configured_cache
  @doc false
  defdelegate channel_create(guild_id, channel), to: @configured_cache
  @doc false
  defdelegate channel_delete(guild_id, channel_id), to: @configured_cache
  @doc false
  defdelegate channel_update(guild_id, channel), to: @configured_cache
  @doc false
  defdelegate emoji_update(guild_id, emojis), to: @configured_cache
  @doc false
  defdelegate role_create(guild_id, role), to: @configured_cache
  @doc false
  defdelegate role_delete(guild_id, role), to: @configured_cache
  @doc false
  defdelegate role_update(guild_id, role), to: @configured_cache
  @doc false
  defdelegate voice_state_update(guild_id, state), to: @configured_cache
  @doc false
  defdelegate member_count_up(guild_id), to: @configured_cache
  @doc false
  defdelegate member_count_down(guild_id), to: @configured_cache

  # Helper functions.

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get!(Guild.id()) :: Guild.t() | no_return()
  def get!(guild_id) do
    guild_id
    |> get
    |> Util.bangify_find(guild_id, __MODULE__)
  end

  @doc """
  Same as `select/2`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec select!(Guild.id(), selector()) :: any() | no_return()
  def select!(guild_id, selector) do
    select(guild_id, selector)
    |> Util.bangify_find(guild_id, __MODULE__)
  end

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

  @doc """
  Return the QLC handle of the configured cache.
  """
  @doc since: "0.8.0"
  defdelegate query_handle(), to: @configured_cache
end
