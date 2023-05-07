defmodule Nostrum.Cache.ChannelCache do
  @default_cache_implementation Nostrum.Cache.ChannelCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for channels outside of guilds.

  You can call the functions provided by this module independent of which cache
  is configured, and it will dispatch to the configured cache implementation.
  The user-facing functions for reading the cache can be found in the "Reading
  the cache" section.

  By default, #{@default_cache_implementation} will be used for caching channels.
  You can override this in the `:caches` option of the `:nostrum` application
  by setting the `:channels` field to a different module implementing the
  `Nostrum.Cache.ChannelCache` behaviour. Any module below
  `Nostrum.Cache.ChannelCache` can be used as a cache.

  ## Writing your own channel cache

  As with the other caches, the channel cache API consists of two parts:

  - The functions that the user calls, currently only `get/1` and `get!/1`

  - The functions that nostrum calls, such as `c:create/1` or `c:update/1`.
  These **do not create any objects in the Discord API**, they are purely
  created to update the cached data from data that Discord sends us. If you
  want to create objects on Discord, use the functions exposed by `Nostrum.Api`
  instead.

  You need to implement both of them for nostrum to work with your custom
  cache. **You also need to implement `Supervisor` callbacks**, which will
  start your cache as a child under `Nostrum.Cache.CacheSupervisor`: As an
  example, the `Nostrum.Cache.ChannelCache.ETS` implementation uses this to to
  set up its ETS table it uses for caching. See the callbacks section for every
  nostrum-related callback you need to implement.

  The "upstream data" wording in this module references the fact that the
  data that the channel cache (and other caches) retrieves represents the raw
  data we receive from the upstream connection, no attempt is made by nostrum
  to sanitize the data before it enters the cache. Caching implementations
  need to cast the data to the resulting type themselves. A possible future
  improvement would be moving the data casting into this module before the
  backing cache implementation is called.
  """

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Message
  alias Nostrum.Util

  @configured_cache :nostrum
                    |> Application.compile_env(
                      [:caches, :channels],
                      @default_cache_implementation
                    )

  @typedoc "Specifies the reason for why a lookup operation has failed."
  @type reason :: :channel_not_found

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate init(init_arg), to: @configured_cache
  @doc false
  defdelegate start_link(init_arg), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

  ## Behaviour specification

  @doc ~S"""
  Retrieves a channel from the cache.

  Internally, the ChannelCache process only stores
  `t:Nostrum.Struct.Channel.dm_channel/0` references. To get channel
  information, a call is made to a `Nostrum.Cache.GuildCache`.

  If successful, returns `{:ok, channel}`. Otherwise, returns `{:error, reason}`

  ## Example
  ```elixir
  case Nostrum.Cache.ChannelCache.get(133333333337) do
    {:ok, channel} ->
      "We found " <> channel.name
    {:error, _reason} ->
      "Donde esta"
  end
  ```
  """
  @doc section: :reading
  @callback get(Channel.id()) :: {:ok, Channel.t()} | {:error, reason}

  # Functions called from nostrum.
  @doc "Create a channel in the cache."
  @callback create(map) :: Channel.t()

  @doc """
  Update a channel from upstream data.

  Return the original channel before the update, and the updated channel.
  """
  @callback update(Channel.t()) :: :noop | {Channel.t(), Channel.t()}

  @doc """
  Delete a channel from the cache.

  Return the old channel if it was cached, or `nil` otherwise.
  """
  @callback delete(Channel.id()) :: :noop | Channel.t()

  @doc """
  Lookup a channel from the cache by ID.

  Return channel_not_found if not found.
  """
  @doc deprecated: "Use ChannelCache.get/1 instead"
  @callback lookup(Channel.id()) :: {:error, reason} | {:ok, map}

  # Dispatching logic

  @doc """
  Look up a channel in the cache, by message or ID.
  """
  def get(%Message{channel_id: channel_id}), do: @configured_cache.get(channel_id)
  defdelegate get(channel_id), to: @configured_cache

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get!(Channel.id() | Nostrum.Struct.Message.t()) :: no_return | Channel.t()
  def get!(%Message{channel_id: channel_id}), do: get!(channel_id)

  def get!(channel_id) do
    channel_id
    |> @configured_cache.get()
    |> Util.bangify_find(channel_id, __MODULE__)
  end

  defdelegate create(map), to: @configured_cache
  defdelegate update(channel), to: @configured_cache
  defdelegate delete(channel_id), to: @configured_cache
  @deprecated "Use ChannelCache.get/1 instead"
  defdelegate lookup(channel_id), to: @configured_cache
end
