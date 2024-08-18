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
  @moduledoc since: "0.10.0"

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
  Retrieves a list of messages from the cache with a given channel ID,
  after a given date, and before a given date.

  Integers should be treated as snowflakes, and the atom `:infinity` when given
  as a before date should be treated as the maximum possible date.
  """
  @callback get_by_channel(
              Channel.id(),
              after_timestamp :: timestamp_like(),
              before_timestamp :: timestamp_like() | :infinity
            ) :: [
              Message.t()
            ]

  @doc """
  Retrieve a list of messages from the cache with a given channel ID and author ID,
  optionally after a given date, and before a given date.
  """
  @callback get_by_channel_and_author(
              Channel.id(),
              User.id(),
              after_timestamp :: timestamp_like(),
              before_timestamp :: timestamp_like() | :infinity
            ) :: [
              Message.t()
            ]

  @doc """
  Retrieve a list of messages from the cache with a given author ID,
  optionally after a given date, and before a given date.

  Integers are treated as snowflakes, and the atom `:infinity` when given
  as a before date will be treated as the maximum possible date.
  """
  @callback get_by_author(
              User.id(),
              after_timestamp :: timestamp_like(),
              before_timestamp :: timestamp_like() | :infinity
            ) :: [
              Message.t()
            ]

  @doc """
  Retrieve the child spec for starting the cache under a supervisor.

  This callback is optional, and if not implemented, the cache will not be
  started under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @typedoc """
  Used to constrain the return values of functions that can return
  a list of messages from the cache.
  """
  @type timestamp_like() :: Snowflake.t() | DateTime.t()

  # User-facing

  @doc """
  Retrieve a message from the cache by channel and message id.
  """
  defdelegate get(message_id), to: @configured_cache

  @doc """
  Retrieve a list of messages from the cache with a given author ID,
  optionally after a given date, and before a given date.

  Integers are treated as snowflakes, and the atom `:infinity` when given
  as a before date will be treated as the maximum possible date.
  """
  defdelegate get_by_author(user_id, after_timestamp \\ 0, before_timestamp \\ :infinity),
    to: @configured_cache

  @doc """
  Retrieve a list of messages from the cache with a given channel ID and author ID,
  optionally after a given date, and before a given date.
  """
  defdelegate get_by_channel_and_author(
                channel_id,
                author_id,
                after_timestamp \\ 0,
                before_timestamp \\ :infinity
              ),
              to: @configured_cache

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

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
