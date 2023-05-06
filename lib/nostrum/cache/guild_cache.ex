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

  - The functions that the user calls, such as `c:all/0` or `c:select_by/2`.

  - The functions that nostrum calls, such as `c:create/1` or `c:update/1`.
  These **do not create any objects in the Discord API**, they are purely
  created to update the cached data from data that Discord sends us. If you
  want to create objects on Discord, use the functions exposed by `Nostrum.Api`
  instead.

  You need to implement both of them for nostrum to work with your custom
  cache. **You also need to implement `Supervisor` callbacks**, which will
  start your cache as a child under `Nostrum.Cache.CacheSupervisor`: As an
  example, the `Nostrum.Cache.GuildCache.ETS` implementation uses this to to
  set up its ETS table it uses for caching. See the callbacks section for every
  nostrum-related callback you need to implement.

  Note that this module also defines a few helper functions, such as `get!/1`
  or `select_by!/2`, which call the backing cache's regular functions and
  perform the result unwrapping by themselves.

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
  alias Nostrum.Struct.Message
  alias Nostrum.Util

  @configured_cache :nostrum
                    |> Application.compile_env([:caches, :guilds], @default_cache_implementation)

  @typedoc "Specifies the reason for why a lookup operation has failed."
  @type reason ::
          :id_not_found
          | :id_not_found_on_guild_lookup

  @typedoc "A selector for looking up entries in the cache."
  @type selector :: (Guild.t() -> any)

  @typedoc "A clause for filtering guilds."
  @type clause ::
          {:id, Guild.id()}
          | {:channel_id, Channel.id()}
          | {:message, Message.t()}

  @typedoc "A collection of `t:clause/0`s for filtering guilds."
  @type clauses :: [clause] | map

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate init(init_arg), to: @configured_cache
  @doc false
  defdelegate start_link(init_arg), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

  ## Behaviour specification

  @doc """
  Retrieves all `Nostrum.Struct.Guild` from the cache.
  """
  @callback all() :: Enum.t()

  @doc """
  Selects guilds matching `selector` from all `Nostrum.Struct.Guild` in the cache.
  """
  @callback select_all(selector :: (Guild.t() -> any())) :: Enum.t()

  @doc """
  Retrieves a single `Nostrum.Struct.Guild` from the cache via its `id`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```elixir
  iex> Nostrum.Cache.GuildCache.get(0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get(10)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @callback get(Guild.id()) :: {:ok, Guild.t()} | {:error, reason}

  @doc """
  Retrieves a single `Nostrum.Struct.Guild` where it matches the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```elixir
  iex> Nostrum.Cache.GuildCache.get_by(id: 0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(%{id: 0})
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(id: 10)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @callback get_by(clauses) :: {:ok, Guild.t()} | {:error, reason()}

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```elixir
  iex> Nostrum.Cache.GuildCache.select(0, fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select(10, fn guild -> guild.id end)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @callback select(Guild.id(), selector) :: {:ok, any} | {:error, reason}

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild` that matches
  the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```elixir
  iex> Nostrum.Cache.GuildCache.select_by([id: 0], fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select_by(%{id: 0}, fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select_by([id: 10], fn guild -> guild.id end)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @callback select_by(clauses, selector) :: {:ok, any} | {:error, reason}

  # Functions called from nostrum.
  @doc "Create a guild in the cache."
  @callback create(Guild.t()) :: true

  @doc """
  Update a guild from upstream data.

  Return the original guild before the update, and the updated guild.
  """
  @callback update(map()) :: {old_guild :: Guild.t(), updated_guild :: Guild.t()}

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

  Return the original channel before the update, and the updated channel.
  """
  @callback channel_update(Guild.id(), channel :: map()) ::
              {old_channel :: Channel.t(), new_channel :: Channel.t()}

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
              {Guild.id(), old_role :: Role.t(), new_role :: Role.t()}

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

  # Dispatching logic.
  defdelegate all, to: @configured_cache
  defdelegate select_all(selector), to: @configured_cache
  defdelegate get(guild_id), to: @configured_cache
  defdelegate get_by(clauses), to: @configured_cache
  defdelegate select(guild_id, selector), to: @configured_cache
  defdelegate select_by(clauses, selector), to: @configured_cache
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
  Same as `get_by/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get_by!(clauses()) :: Guild.t() | no_return()
  def get_by!(clauses) do
    clauses
    |> get_by
    |> Util.bangify_find(clauses, __MODULE__)
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
  Same as `select_by/2`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec select_by!(clauses(), selector()) :: any() | no_return()
  def select_by!(clauses, selector) do
    select_by(clauses, selector)
    |> Util.bangify_find(clauses, __MODULE__)
  end
end
