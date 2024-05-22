defmodule Nostrum.Cache.MessageCache do
  @default_cache_implementation Nostrum.Cache.MessageCache.Noop

  @moduledoc """
  Cache behaviour & dispatcher for Discord messages.

  By default, #{@default_cache_implementation} will be used for caching
  messages. You can override this in the `:caches` option of the `nostrum`
  application by setting the `:messages` field to a different module, or
  to the tuple `{module, config}` where `module` is the module to use and
  `config` is any compile-time configuration to pass to the module.

  Unlike the other caches, the default is a no-op cache, as messages take
  up a lot of memory and most bots do not need messages to be cached.
  If you would like to cache messages, you can change the cache implementation
  to one of the provided modules under `Nostrum.Cache.MessageCache`
  or write your own.

  ## Writing your own message cache

  As with the other caches, the message cache API consists of two parts:

  - The functions that nostrum calls, such as `c:create/1` or `c:update/1`.
  These **do not create any objects in the Discord API**, they are purely
  created to update the cached data from data that Discord sends us. If you
  want to create objects on Discord, use the functions exposed by `Nostrum.Api` instead.

  - the `c:child_spec/1` callback for starting the cache under a supervisor.

  You need to implement both of them for nostrum to work with your custom
  cache.
  """

  @configured_cache Nostrum.Cache.Base.get_cache_module(:messages, @default_cache_implementation)

  alias Nostrum.Snowflake
  alias Nostrum.Struct.{Channel, Message, User}

  # callbacks

  @doc """
  Retrieve a single `Nostrum.Struct.Message` from the cache by its ID.
  """
  @callback get(Message.id()) :: {:ok, Message.t()} | {:error, :not_found}

  @doc """
  Creates a message in the cache.

  The argument given is the raw message payload from Discord's gateway.
  """
  @callback create(map()) :: Message.t()

  @doc """
  Updates a message in the cache.

  The argument given is the raw message payload from Discord's gateway,
  and the return value is a tuple of the updated message and the old message if
  it was found in the cache, otherwise `nil`.
  """
  @callback update(map()) :: {old_message :: Message.t() | nil, updated_message :: Message.t()}

  @doc """
  Deletes a message from the cache.

  Expects the deleted message to be returned if it was found.
  """
  @callback delete(Channel.id(), Message.id()) :: Message.t() | nil

  @doc """
  Deletes multiple messages from the cache, any message IDs given
  will always be for the same channel.

  Returns a list of the deleted messages.
  Note that if a message was not found in the cache, it will
  not be included in the returned list.
  """
  @callback bulk_delete(Channel.id(), [Message.id()]) :: [Message.t()]

  @doc """
  Called when a channel is deleted.

  Any messages in the cache for the channel should be removed.
  """
  @callback channel_delete(Channel.id()) :: :ok

  @doc """
  Return a QLC query handle for the cache for read operations.

  This is used by nostrum to provide any read operations on the cache. Write
  operations still need to be implemented separately.

  The Erlang manual on [Implementing a QLC
  Table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table)
  contains examples for implementation. To prevent full table scans, accept
  match specifications in your `TraverseFun` and implement a `LookupFun` as
  documented.

  The query handle must return items in the form `{channel_id, author_id, message}`, where:
  - `channel_id` is a `t:Nostrum.Struct.Channel.id/0`,
  - `author_id` is a `t:Nostrum.Struct.User.id/0`, and
  - `message` is a `t:Nostrum.Struct.Message.t/0`.

  If your cache needs some form of setup or teardown for QLC queries (such as
  opening connections), see `c:wrap_qlc/1`.
  """
  @callback query_handle() :: :qlc.query_handle()

  @doc """
  Retrieve the child spec for starting the cache under a supervisor.

  This callback is optional, and if not implemented, the cache will not be
  started under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  A function that should wrap any `:qlc` operations.

  If you implement a cache that is backed by a database and want to perform
  cleanup and teardown actions such as opening and closing connections,
  managing transactions and so on, you want to implement this function. nostrum
  will then effectively call `wrap_qlc(fn -> :qlc.e(...) end)`.

  If your cache does not need any wrapping, you can omit this.
  """
  @callback wrap_qlc((-> result)) :: result when result: term()
  @optional_callbacks wrap_qlc: 1

  @typedoc """
  Used to constrain the return values of functions that can return
  a list of messages from the cache.
  """
  @type timestamp_like :: integer() | DateTime.t()

  # User-facing

  @doc """
  Retrieve a message from the cache by channel and message id.
  """
  defdelegate get(message_id), to: @configured_cache

  @doc """
  Retrieve a list of messages from the cache with a given channel ID,
  after a given date, and before a given date.

  Integers are treated as snowflakes, and the atom `:infinity` when given
  as a before date will be treated as the maximum possible date.
  """
  @spec get_by_channel(Channel.id(), timestamp_like(), timestamp_like() | :infinity) :: [
          Message.t()
        ]
  def get_by_channel(
        channel_id,
        after_timestamp \\ 0,
        before_timestamp \\ :infinity,
        cache \\ @configured_cache
      ) do
    after_timestamp = timestamp_like_to_snowflake(after_timestamp)
    before_timestamp = timestamp_like_to_snowflake(before_timestamp)

    unsorted_result =
      wrap_qlc(cache, fn ->
        :nostrum_message_cache_qlc.by_channel(
          channel_id,
          after_timestamp,
          before_timestamp,
          cache
        )
        |> :qlc.e()
      end)

    Enum.sort_by(unsorted_result, & &1.id)
  end

  @doc """
  Retrieve a list of messages from the cache with a given author ID,
  optionally after a given date, and before a given date.

  Integers are treated as snowflakes, and the atom `:infinity` when given
  as a before date will be treated as the maximum possible date.
  """
  @spec get_by_author(User.id(), timestamp_like(), timestamp_like() | :infinity) :: [
          Message.t()
        ]
  def get_by_author(
        author_id,
        after_timestamp \\ 0,
        before_timestamp \\ :infinity,
        cache \\ @configured_cache
      ) do
    after_timestamp = timestamp_like_to_snowflake(after_timestamp)
    before_timestamp = timestamp_like_to_snowflake(before_timestamp)

    unsorted_result =
      wrap_qlc(cache, fn ->
        :nostrum_message_cache_qlc.by_author(
          author_id,
          after_timestamp,
          before_timestamp,
          cache
        )
        |> :qlc.e()
      end)

    Enum.sort_by(unsorted_result, & &1.id)
  end

  @doc """
  Retrieve a list of messages from the cache with a given channel ID and author ID,
  optionally after a given date, and before a given date.
  """
  @spec get_by_channel_and_author(
          Channel.id(),
          User.id(),
          timestamp_like(),
          timestamp_like() | :infinity
        ) :: [
          Message.t()
        ]
  def get_by_channel_and_author(
        channel_id,
        author_id,
        after_timestamp \\ 0,
        before_timestamp \\ :infinity,
        cache \\ @configured_cache
      ) do
    after_timestamp = timestamp_like_to_snowflake(after_timestamp)
    before_timestamp = timestamp_like_to_snowflake(before_timestamp)

    unsorted_result =
      wrap_qlc(cache, fn ->
        :nostrum_message_cache_qlc.by_channel_and_author(
          channel_id,
          author_id,
          after_timestamp,
          before_timestamp,
          cache
        )
        |> :qlc.e()
      end)

    Enum.sort_by(unsorted_result, & &1.id)
  end

  @doc """
  Call `c:wrap_qlc/1` on the given cache, if implemented.

  If no cache is given, calls out to the default cache.
  """
  @spec wrap_qlc((-> result)) :: result when result: term()
  @spec wrap_qlc(module(), (-> result)) :: result when result: term()
  def wrap_qlc(cache \\ @configured_cache, fun) do
    if function_exported?(cache, :wrap_qlc, 1) do
      cache.wrap_qlc(fun)
    else
      fun.()
    end
  end

  # Nostrum-facing

  @doc false
  defdelegate create(message), to: @configured_cache
  @doc false
  defdelegate update(message), to: @configured_cache
  @doc false
  defdelegate delete(channel_id, message_id), to: @configured_cache
  @doc false
  defdelegate bulk_delete(channel_id, message_ids), to: @configured_cache
  @doc false
  defdelegate channel_delete(channel_id), to: @configured_cache

  @doc """
  Return the QLC handle of the configured cache.
  """
  defdelegate query_handle(), to: @configured_cache

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

  defp timestamp_like_to_snowflake(:infinity), do: :infinity
  defp timestamp_like_to_snowflake(snowflake) when is_integer(snowflake), do: snowflake

  defp timestamp_like_to_snowflake(%DateTime{} = dt) do
    case Snowflake.from_datetime(dt) do
      {:ok, snowflake} -> snowflake
      # The date we got was before Discord's epoch, so we'll just treat it as 0
      :error -> 0
    end
  end
end
