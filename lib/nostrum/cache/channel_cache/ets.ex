defmodule Nostrum.Cache.ChannelCache.ETS do
  @moduledoc """
  An ETS-based cache for channels outside of guilds.

  The supervisor defined by this module will set up the ETS table associated
  with it.

  If you need to get the table reference for the table used by this module,
  please use the `table/0` function.

  Note that users should not call the functions not related to this specific
  implementation of the cache directly. Instead, call the functions of
  `Nostrum.Cache.ChannelCache` directly, which will dispatch to the configured
  cache.
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.ChannelCache

  @table_name :nostrum_channels

  alias Nostrum.Cache.ChannelCache
  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Channel
  import Nostrum.Snowflake, only: [is_snowflake: 1]
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

  # IMPLEMENTATION
  @doc "Retrieve a channel from the cache by ID."
  @impl ChannelCache
  @spec get(Channel.id()) :: {:ok, Channel.t()} | {:error, ChannelCache.reason()}
  def get(id) when is_snowflake(id) do
    case lookup(id) do
      {:ok, channel} -> {:ok, convert(channel)}
      error -> error
    end
  end

  @doc "Converts and creates the given map as a channel in the cache."
  @impl ChannelCache
  @spec create(map) :: Channel.t()
  def create(channel) do
    :ets.insert(@table_name, {channel.id, channel})
    convert(channel)
  end

  @doc "Update the given channel in the cache."
  @impl ChannelCache
  @spec update(Channel.t()) :: :noop | {Channel.t(), Channel.t()}
  def update(channel) do
    case lookup(channel.id) do
      {:ok, old_channel} ->
        # XXX: this is incorrect. we always keep the old channel around.
        # but updating it everytime would be wrong as well. we need to know
        # the source that `lookup/1` got it from.
        {convert(old_channel), convert(channel)}

      _ ->
        :noop
    end
  end

  @doc "Delete the channel from the cache by ID."
  @impl ChannelCache
  @spec delete(Channel.id()) :: :noop | Channel.t()
  def delete(id) do
    case lookup(id) do
      {:ok, channel} ->
        :ets.delete(@table_name, id)
        convert(channel)

      _ ->
        :noop
    end
  end

  @doc "Lookup a channel from the cache by ID and if it does not exist try to get from GuildCache with select_by"
  @impl ChannelCache
  @spec lookup(Channel.id()) :: {:error, :channel_not_found} | {:ok, map}
  def lookup(id) do
    case :ets.lookup(@table_name, id) do
      [] ->
        [channel_id: id]
        |> GuildCache.select_by(fn %{channels: channels} ->
          Map.get(channels, id, {:error, :id_not_found})
        end)
        |> case do
          {:error, :id_not_found_on_guild_lookup} ->
            {:error, :channel_not_found}

          res ->
            res
        end

      [{^id, channel}] ->
        {:ok, channel}
    end
  end

  # Converts a map into a Channel
  defp convert(%{__struct__: _} = struct), do: struct
  defp convert(map), do: Channel.to_struct(map)
end
