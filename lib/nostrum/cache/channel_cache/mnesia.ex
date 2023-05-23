if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.ChannelCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for channels outside of guilds.

    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](State.md) documentation
    for more information.

    To retrieve the table name used by this cache, use `table/0`.
    """
    @moduledoc since: "0.8.0"

    @table_name :nostrum_channels
    @record_name @table_name

    @behaviour Nostrum.Cache.ChannelCache

    alias Nostrum.Cache.ChannelCache
    alias Nostrum.Struct.Channel
    use Supervisor

    @doc "Retrieve the table name used by the cache."
    @spec table :: atom()
    def table, do: @table_name

    @doc "Clear any objects in the cache."
    @spec clear() :: {:atomic, :ok} | {:aborted, term()}
    def clear, do: :mnesia.clear_table(@table_name)

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(@table_name)

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = [attributes: [:id, :data], record_name: @record_name]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @impl ChannelCache
    @doc "Creates the given channel in the cache."
    @spec create(map) :: Channel.t()
    def create(channel) do
      parsed = convert(channel)
      record = {@record_name, channel.id, parsed}
      {:atomic, :ok} = :mnesia.sync_transaction(fn -> :mnesia.write(record) end)
      parsed
    end

    @impl ChannelCache
    @doc "Update the given channel in the cache."
    @spec update(Channel.t()) :: {Channel.t() | nil, Channel.t()}
    def update(channel) do
      parsed = convert(channel)

      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, channel.id, :write) do
          [{_tag, id, old_channel}] ->
            :mnesia.write(@table_name, {@record_name, id, parsed}, :write)
            {old_channel, parsed}

          [] ->
            {nil, parsed}
        end
      end)
    end

    @impl ChannelCache
    @doc "Delete the channel from the cache by ID."
    @spec delete(Channel.id()) :: :noop | Channel.t()
    def delete(id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, id, :write) do
          [{_tag, _id, channel}] ->
            :mnesia.delete({@table_name, id})
            channel

          [] ->
            :noop
        end
      end)
    end

    @impl ChannelCache
    @doc "Retrieve a QLC query handle for the channel cache."
    @doc since: "0.8.0"
    @spec query_handle :: :qlc.query_handle()
    def query_handle do
      ms = [{{:_, :"$1", :"$2"}, [], [{{:"$1", :"$2"}}]}]
      :mnesia.table(@table_name, {:traverse, {:select, ms}})
    end

    @impl ChannelCache
    @doc "Wrap QLC operations in a transaction."
    @doc since: "0.8.0"
    def wrap_qlc(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end

    # Converts a map into a Channel
    defp convert(%{__struct__: _} = struct), do: struct
    defp convert(map), do: Channel.to_struct(map)
  end
end
