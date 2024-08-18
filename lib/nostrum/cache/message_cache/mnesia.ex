if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.MessageCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for messages.

    #{Nostrum.Cache.Base.mnesia_note()}

    By default, the cache will store up to `10_000` messages,
    and will evict the `100` oldest messages when the limit is reached.

    The reason for the eviction count is that with mnesia it is more efficient to
    find X oldest records and delete them all at once than to find the oldest
    record and delete it each time a new record is added.

    The Mnesia cache supports the following configuration options:
    - `size_limit`: The maximum number of messages to store in the cache.
    default: `10_000`
    - `eviction_count`: The number of messages to evict when the cache is full.
    default: `100`
    - `table_name`: The name of the Mnesia table to use for the cache.
    default: `:nostrum_messages`
    - `compressed`: Whether to use compressed in memory storage for the table.
    default: `false`
    - `type`: Sets the type of Mnesia table created to cache messages.
    Can be either `:set` or `:ordered_set`, by choosing `:ordered_set` the
    eviction of the oldest messages will be more efficient, however it means
    that the table cannot be changed to only store its contents on disk later.
    default: `:ordered_set`

    To change this configuration, you can add the following to your
    `config.exs`:

    ```elixir
    config :nostrum,
      caches: %{
        messages: {Nostrum.Cache.MessageCache.Mnesia,
                   size_limit: 1000, eviction_count: 50,
                   table_name: :my_custom_messages_table_name,
                   compressed: true, type: :set}
      }
    ```

    You can also change the table name used by the cache by setting the
    `table_name` field in the configuration for the `messages` cache.
    """
    @moduledoc since: "0.10.0"

    @config Nostrum.Cache.Base.get_cache_options(:messages)

    # allow us to override the table name for testing
    # without accidentally overwriting the production table
    @table_name @config[:table_name] || :nostrum_messages
    @record_name @table_name

    @maximum_size @config[:size_limit] || 10_000
    @eviction_count @config[:eviction_count] || 100
    @compressed_table @config[:compressed] || false
    @table_type @config[:type] || :ordered_set

    @behaviour Nostrum.Cache.MessageCache

    alias Nostrum.Cache.MessageCache
    alias Nostrum.Snowflake
    alias Nostrum.Struct.Channel
    alias Nostrum.Struct.Message
    alias Nostrum.Struct.User
    alias Nostrum.Util
    use Supervisor

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = table_create_attributes()

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
        maybe_evict_records()
        :mnesia.write(record)
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
            # save it in the cache as updates are not guaranteed
            # to have the full message payload
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
    @spec delete(Channel.id(), Message.id()) :: Message.t() | nil
    def delete(channel_id, message_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, message_id, :write) do
          # as a safety measure, we check the channel_id
          # before deleting the message from the cache
          # to prevent deleting messages from the wrong channel
          [{_tag, _id, ^channel_id, _author_id, message}] ->
            :mnesia.delete(@table_name, message_id, :write)
            message

          _ ->
            nil
        end
      end)
    end

    @impl MessageCache
    @doc """
    Removes and returns a list of messages from the cache.
    Messages not found in the cache will not be included in the returned list.
    """
    @spec bulk_delete(Channel.id(), [Message.id()]) :: [Message.t()]
    def bulk_delete(channel_id, message_ids) do
      Enum.reduce(message_ids, [], fn message_id, list ->
        case delete(channel_id, message_id) do
          nil ->
            list

          message ->
            [message | list]
        end
      end)
      |> Enum.reverse()
    end

    @impl MessageCache
    @doc "Removes all messages for a channel which was deleted."
    @spec channel_delete(Channel.id()) :: :ok
    def channel_delete(channel_id) do
      :mnesia.activity(:sync_transaction, fn ->
        handle = :nostrum_message_cache_qlc.all_message_ids_in_channel(channel_id, query_handle())

        :qlc.fold(
          fn message_id, _ ->
            :mnesia.delete(@table_name, message_id, :write)
          end,
          nil,
          handle
        )
      end)

      :ok
    end

    @impl MessageCache
    @doc """
    Retrieve a list of messages from the cache with a given channel ID,
    optionally after a given date, and before a given date.
    """
    @spec get_by_channel(
            Channel.id(),
            MessageCache.timestamp_like(),
            MessageCache.timestamp_like() | :infinity
          ) :: [
            Message.t()
          ]
    def get_by_channel(
          channel_id,
          after_timestamp \\ 0,
          before_timestamp \\ :infinity
        ) do
      after_timestamp = Util.timestamp_like_to_snowflake(after_timestamp)
      before_timestamp = Util.timestamp_like_to_snowflake(before_timestamp)

      :mnesia.activity(:sync_transaction, fn ->
        :nostrum_message_cache_qlc.by_channel(
          channel_id,
          after_timestamp,
          before_timestamp,
          query_handle()
        )
        |> :qlc.e()
      end)
      |> Enum.sort_by(& &1.id)
    end

    @impl MessageCache
    @doc """
    Retrieve a list of messages from the cache with a given channel ID and author ID,
    optionally after a given date, and before a given date.
    """
    @spec get_by_channel_and_author(
            Channel.id(),
            User.id(),
            MessageCache.timestamp_like(),
            MessageCache.timestamp_like() | :infinity
          ) :: [
            Message.t()
          ]
    def get_by_channel_and_author(
          channel_id,
          author_id,
          after_timestamp \\ 0,
          before_timestamp \\ :infinity
        ) do
      after_timestamp = Util.timestamp_like_to_snowflake(after_timestamp)
      before_timestamp = Util.timestamp_like_to_snowflake(before_timestamp)

      :mnesia.activity(:sync_transaction, fn ->
        :nostrum_message_cache_qlc.by_channel_and_author(
          channel_id,
          author_id,
          after_timestamp,
          before_timestamp,
          query_handle()
        )
        |> :qlc.e()
      end)
      |> Enum.sort_by(& &1.id)
    end

    @impl MessageCache
    @doc """
    Retrieve a list of messages from the cache with a given author ID,
    optionally after a given date, and before a given date.

    Integers are treated as snowflakes, and the atom `:infinity` when given
    as a before date will be treated as the maximum possible date.
    """
    @spec get_by_author(
            User.id(),
            MessageCache.timestamp_like(),
            MessageCache.timestamp_like() | :infinity
          ) :: [
            Message.t()
          ]
    def get_by_author(
          author_id,
          after_timestamp \\ 0,
          before_timestamp \\ :infinity
        ) do
      after_timestamp = Util.timestamp_like_to_snowflake(after_timestamp)
      before_timestamp = Util.timestamp_like_to_snowflake(before_timestamp)

      :mnesia.activity(:sync_transaction, fn ->
        :nostrum_message_cache_qlc.by_author(
          author_id,
          after_timestamp,
          before_timestamp,
          query_handle()
        )
        |> :qlc.e()
      end)
      |> Enum.sort_by(& &1.id)
    end

    @doc "Return a QLC query handle for the cache for read operations."
    @spec query_handle() :: :qlc.query_handle()
    def query_handle do
      :mnesia.table(@table_name)
    end

    @doc false
    def table_create_attributes do
      ets_props =
        if @compressed_table do
          [:compressed]
        else
          []
        end

      [
        attributes: [:message_id, :channel_id, :author_id, :data],
        index: [:channel_id, :author_id],
        record_name: @record_name,
        storage_properties: [ets: ets_props],
        type: @table_type
      ]
    end

    # assumes its called from within a transaction
    defp maybe_evict_records do
      size = :mnesia.table_info(@table_name, :size)

      if size >= @maximum_size do
        case :mnesia.table_info(@table_name, :type) do
          :set ->
            evict_set_records()

          :ordered_set ->
            evict_ordered_set_records()
        end
      end
    end

    defp evict_set_records do
      {_, set, _} =
        :mnesia.foldl(
          &evict_set_fold_func/2,
          {0, :gb_sets.new(), 0},
          @table_name
        )

      ids = :gb_sets.to_list(set)

      Enum.each(ids, fn message_id ->
        :mnesia.delete(@table_name, message_id, :write)
      end)
    end

    defp evict_set_fold_func(
           {_tag, message_id, _channel_id, _author_id, _message},
           {count, set, largest}
         ) do
      cond do
        message_id < largest and count >= @eviction_count ->
          set = :gb_sets.delete(largest, set)
          set = :gb_sets.insert(message_id, set)
          largest = :gb_sets.largest(set)
          {count, set, largest}

        count < @eviction_count ->
          set = :gb_sets.insert(message_id, set)
          largest = :gb_sets.largest(set)
          {count + 1, set, largest}

        true ->
          {count, set, largest}
      end
    end

    defp evict_ordered_set_records do
      first = :mnesia.first(@table_name)

      Enum.reduce(1..(@eviction_count - 1), [first], fn _i, [key | _rest] = list ->
        next_key = :mnesia.next(@table_name, key)
        [next_key | list]
      end)
      |> Enum.each(fn key ->
        :mnesia.delete(@table_name, key, :write)
      end)
    end
  end
end
