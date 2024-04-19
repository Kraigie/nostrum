defmodule Nostrum.Cache.DMChannelCache do
  @default_cache_implementation Nostrum.Cache.DMChannelCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for direct message channels.

  You can call the functions provided by this module independent of which cache
  is configured, and it will dispatch to the configured cache implementation.
  The user-facing functions for reading the cache can be found in the "Reading
  the cache" section.

  > ### Channels vs. DM Channels {: .info}
  >
  > This module only stores cached copies of direct message channels. All other
  > channels will be attributed to a guild. You can map a channel ID to a guild
  > with the `Nostrum.Cache.ChannelGuildMapping` cache, once you have a guild
  > ID, you can look at the `t:Nostrum.Struct.Guild.channels/0` map to
  > find your channel.

  By default, #{@default_cache_implementation} will be used for caching DM channels.
  You can override this in the `:caches` option of the `:nostrum` application
  by setting the `:dm_channels` field to a different module implementing the
  `Nostrum.Cache.DMChannelCache` behaviour. Any module below
  `Nostrum.Cache.DMChannelCache` can be used as a cache.

  ## Writing your own DM channel cache

  As with the other caches, the channel cache API consists of three parts:

  - Functions called by nostrum, such as `c:create/1` or `c:update/1`. These
  **do not create any objects in the Discord API**, they are purely created to
  update the cached data from data that Discord sends us. If you want to create
  objects on Discord, use the functions exposed by `Nostrum.Api` instead.

  - the QLC query handle for read operations, `c:query_handle/0`, and

  - the `c:child_spec/1` callback for starting the cache under a supervisor.

  You need to implement all of them for nostrum to work with your custom
  cache.

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
                      [:caches, :dm_channels],
                      @default_cache_implementation
                    )

  @typedoc "Specifies the reason for why a lookup operation has failed."
  @type reason :: :not_found

  ## Behaviour specification

  # Functions called from nostrum.
  @doc "Create a DM channel in the cache."
  @callback create(map) :: Channel.t()

  @doc """
  Update a DM channel from upstream data.

  Return the original channel before the update, and the updated channel.
  """
  @callback update(Channel.t()) :: {Channel.t() | nil, Channel.t()}

  @doc """
  Delete a DM channel from the cache.

  Return the old channel if it was cached, or `nil` otherwise.
  """
  @callback delete(Channel.id()) :: :noop | Channel.t()

  @doc """
  Return a QLC query handle for cache read operations.

  The Erlang manual on [Implementing a QLC
  Table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table)
  contains examples for implementation. To prevent full table scans, accept
  match specifications in your `TraverseFun` and implement a `LookupFun` as
  documented.

  The query handle must return items in the form `{channel_id, channel}`, where:
  - `channel_id` is a `t:Nostrum.Struct.Channel.id/0`, and
  - `channel` is a `t:Nostrum.Struct.Channel.t/0`

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
  @callback wrap_qlc((-> result)) :: result when result: term()
  @optional_callbacks wrap_qlc: 1

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @doc since: "0.8.0"
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  Look up a DM channel in the cache, by message or ID.

  An optional second argument can be passed to select the cache to read from.
  """
  @spec get(Channel.id() | Message.t()) :: {:ok, Channel.t()} | {:error, reason}
  @spec get(Channel.id() | Message.t(), module()) :: {:ok, Channel.t()} | {:error, reason}
  def get(channel_or_message), do: get(channel_or_message, @configured_cache)
  def get(%Message{channel_id: channel_id}, cache), do: get(channel_id, cache)

  def get(channel_id, cache) do
    handle = :nostrum_dmchannel_cache_qlc.get(channel_id, cache)

    wrap_qlc(cache, fn ->
      case :qlc.eval(handle) do
        [{_channel, channel}] ->
          {:ok, channel}

        [] ->
          {:error, :not_found}
      end
    end)
  end

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get!(Channel.id() | Nostrum.Struct.Message.t()) :: no_return | Channel.t()
  @spec get!(Channel.id() | Nostrum.Struct.Message.t(), module()) :: no_return | Channel.t()
  def get!(channel_or_message), do: get!(channel_or_message, @configured_cache)
  def get!(%Message{channel_id: channel_id}, cache), do: get!(channel_id, cache)

  def get!(channel_id, cache) do
    channel_id
    |> get(cache)
    |> Util.bangify_find(channel_id, __MODULE__)
  end

  @doc """
  Return the QLC handle of the configured cache.
  """
  @doc since: "0.8.0"
  defdelegate query_handle(), to: @configured_cache

  @spec wrap_qlc(module(), (-> result)) :: result when result: term()
  defp wrap_qlc(cache, fun) do
    if function_exported?(cache, :wrap_qlc, 1) do
      cache.wrap_qlc(fun)
    else
      fun.()
    end
  end

  # Nostrum dispatch
  @doc false
  defdelegate create(map), to: @configured_cache
  @doc false
  defdelegate update(channel), to: @configured_cache
  @doc false
  defdelegate delete(channel_id), to: @configured_cache

  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
