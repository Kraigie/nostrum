if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.MessageCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for messages.

    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](functionality/state.md)
    documentation for more information.

    To retrieve the table name used by this cache, use `table/0`.

    By default, the cache will store up to 10,000 messages.
    To change this limit, add the `:message_cache_size_limit` key to the `:caches`
    key in your Nostrum compile-time configuration.

    When the cache reaches its limit, the 100 oldest messages will be removed
    to make room for new messages.
    To change the number of messages removed, add the `:message_cache_eviction_count`
    key to the `:caches` key in your Nostrum compile-time configuration.

    The reason for this is that `:qlc` queries do not optimize for sort + limit
    operations, so a full table scan + sort is required to find the oldest messages.
    """

    @table_name :nostrum_messages
    @record_name @table_name

    @maximum_size :nostrum
                  |> Application.compile_env([:caches, :message_cache_size_limit], 10_000)
    @eviction_count :nostrum
                    |> Application.compile_env([:caches, :message_cache_eviction_count], 100)

    @behaviour Nostrum.Cache.MessageCache

    alias Nostrum.Cache.MessageCache
    alias Nostrum.Snowflake
    alias Nostrum.Struct.Channel
    alias Nostrum.Struct.Message
    alias Nostrum.Util
    use Supervisor

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = [
        attributes: [:message_id, :channel_id, :author_id, :data],
        index: [:channel_id, :author_id],
        record_name: @record_name
      ]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @doc "Retrieve the Mnesia table name used for the cache."
    @spec table :: atom()
    def table, do: @table_name

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(@table_name)

    @doc "Clear any objects in the cache."
    @spec clear() :: :ok
    def clear do
      {:atomic, :ok} = :mnesia.clear_table(@table_name)
      :ok
    end

    # Used by dispatch

    @impl MessageCache
    @doc "Retrieve a single message from the cache by id."
    @spec get(Message.id()) :: {:ok, Message.t()} | {:error, :not_found}
    def get(message_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, message_id, :read) do
          [{_tag, _message_id, _channel_id, _author_id, message}] ->
            {:ok, message}

          _ ->
            {:error, :not_found}
        end
      end)
    end

    @impl MessageCache
    @doc "Adds a message to the cache."
    @spec create(map()) :: Message.t()
    def create(payload) do
      message = Message.to_struct(payload)

      record =
        {@record_name, message.id, message.channel_id, message.author.id, message}

      writer = fn ->
        size = :mnesia.table_info(@table_name, :size)

        if size >= @maximum_size do
          oldest_message_ids =
            :nostrum_message_cache_qlc.sorted_by_age_with_limit(__MODULE__, @eviction_count)

          Enum.each(oldest_message_ids, fn message_id ->
            :mnesia.delete(@table_name, message_id, :write)
          end)

          :mnesia.write(record)
        end
      end

      {:atomic, :ok} = :mnesia.sync_transaction(writer)
      message
    end

    @impl MessageCache
    @doc "Updates a message in the cache."
    @spec update(map()) :: {old_message :: Message.t() | nil, updated_message :: Message.t()}
    def update(payload) do
      atomized_payload =
        payload
        |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

      %{id: id} = atomized_payload
      id = Snowflake.cast!(id)

      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, id, :write) do
          [] ->
            # we don't have the old message, so we shouldn't
            # save it in the cache
            updated_message = Message.to_struct(atomized_payload)
            {nil, updated_message}

          [{_tag, _message_id, _channel_id, _author_id, old_message} = entry] ->
            updated_message = Message.to_struct(atomized_payload, old_message)

            :mnesia.write(put_elem(entry, 4, updated_message))
            {old_message, updated_message}
        end
      end)
    end

    @impl MessageCache
    @doc "Removes a message from the cache."
    @spec delete(Channel.id(), Message.id()) :: Message.t() | :noop
    def delete(channel_id, message_id) do
      key = {channel_id, message_id}

      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, key, :write) do
          [{_tag, _key, _channel_id, _author_id, message}] ->
            :mnesia.delete(@table_name, key, :write)
            message

          _ ->
            :noop
        end
      end)
    end

    @impl MessageCache
    @doc "Removes and returns a list of messages from the cache."
    @spec bulk_delete(Channel.id(), [Message.id()]) :: [Message.t()]
    def bulk_delete(channel_id, message_ids) do
      Enum.map(message_ids, fn message_id ->
        case delete(channel_id, message_id) do
          :noop ->
            Message.to_struct(%{id: message_id, channel_id: channel_id})

          message ->
            message
        end
      end)
    end

    @impl MessageCache
    @doc "Removes all messages for a channel which was deleted."
    @spec channel_delete(Channel.id()) :: :ok
    def channel_delete(channel_id) do
      :mnesia.activity(:sync_transaction, fn ->
        :mnesia.index_read(@table_name, channel_id, :channel_id)
        |> Enum.each(fn {_tag, message_id, _channel_id, _author_id, _data} ->
          :mnesia.delete(@table_name, message_id, :write)
        end)
      end)

      :ok
    end

    @impl MessageCache
    @doc "Return a QLC query handle for the cache for read operations."
    @spec query_handle() :: :qlc.query_handle()
    def query_handle do
      :mnesia.table(@table_name)
    end

    @impl MessageCache
    @doc "Wrap QLC operations in a transaction"
    def wrap_qlc(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
