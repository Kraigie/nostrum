defmodule Nostrum.Cache.DMChannelCache.ETS do
  @moduledoc """
  An ETS-based cache for channels outside of guilds.

  The supervisor defined by this module will set up the ETS table associated
  with it.

  If you need to get the table reference for the table used by this module,
  please use the `table/0` function.

  Note that users should not call the functions not related to this specific
  implementation of the cache directly. Instead, call the functions of
  `Nostrum.Cache.DMChannelCache` directly, which will dispatch to the configured
  cache.
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.DMChannelCache

  @table_name :nostrum_channels

  alias Nostrum.Cache.DMChannelCache
  alias Nostrum.Struct.Channel
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the cache's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the ETS table reference used for the cache."
  @doc since: "0.8.0"
  @spec table :: :ets.table()
  def table, do: @table_name

  @doc "Converts and creates the given map as a channel in the cache."
  @impl DMChannelCache
  @spec create(map) :: Channel.t()
  def create(channel) do
    parsed = convert(channel)
    :ets.insert(@table_name, {channel.id, parsed})
    parsed
  end

  @doc "Update the given channel in the cache."
  @impl DMChannelCache
  @spec update(Channel.t()) :: {Channel.t() | nil, Channel.t()}
  def update(channel) do
    parsed = convert(channel)

    case :ets.lookup(@table_name, channel.id) do
      [{_id, old_channel}] ->
        :ets.insert(@table_name, {channel.id, parsed})
        {old_channel, parsed}

      [] ->
        {nil, parsed}
    end
  end

  @doc "Delete the channel from the cache by ID."
  @impl DMChannelCache
  @spec delete(Channel.id()) :: :noop | Channel.t()
  def delete(id) do
    case :ets.lookup(@table_name, id) do
      [{_id, channel}] ->
        :ets.delete(@table_name, id)
        channel

      [] ->
        :noop
    end
  end

  @impl DMChannelCache
  @doc "Retrieve a query handle for usage with QLC."
  @doc since: "0.8.0"
  @spec query_handle() :: :qlc.query_handle()
  def query_handle, do: :ets.table(@table_name)

  # Converts a map into a Channel
  defp convert(%{__struct__: _} = struct), do: struct
  defp convert(map), do: Channel.to_struct(map)
end
