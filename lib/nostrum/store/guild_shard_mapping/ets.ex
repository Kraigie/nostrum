defmodule Nostrum.Store.GuildShardMapping.ETS do
  @moduledoc """
  Maintains a mapping of guild IDs to their shard numbers using `:ets`.

  If programmatic access to the ETS table is needed, please use the `table/0`
  function.

  Please do not use this module directly, apart from special functions such as
  `table/0`. Use `Nostrum.Store.GuildShardMapping` to call the configured
  mapping instead.
  """
  @moduledoc since: "0.8.0"

  alias Nostrum.Store.GuildShardMapping
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.WSState

  @behaviour GuildShardMapping

  @table_name :nostrum_guild_shard_map

  use Supervisor

  @doc "Retrieve the ETS table reference used for the store."
  @spec table :: :ets.table()
  def table, do: @table_name

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the store's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    _tid = :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl GuildShardMapping
  @doc "Create a new mapping for the given guild ID to the given shard ID."
  @spec create(Guild.id(), WSState.shard_num()) :: :ok
  def create(guild_id, shard_num) do
    :ets.insert(@table_name, {guild_id, shard_num})
    :ok
  end

  @impl GuildShardMapping
  @doc "Delete any stored mapping for the given guild ID."
  @spec delete(Guild.id()) :: :ok
  def delete(guild_id) do
    :ets.delete(@table_name, guild_id)
    :ok
  end

  @impl GuildShardMapping
  @doc "Get the shard number for the given guild ID."
  @spec get(Guild.id()) :: WSState.shard_num() | nil
  def get(guild_id) do
    case :ets.lookup(@table_name, guild_id) do
      [{_guild_id, shard_num}] -> shard_num
      [] -> nil
    end
  end
end
