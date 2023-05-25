if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Store.GuildShardMapping.Mnesia do
    @moduledoc """
    Maintains a mapping of guild IDs to their shard numbers using Mnesia.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    alias Nostrum.Store.GuildShardMapping
    alias Nostrum.Struct.Guild
    alias Nostrum.Struct.WSState

    @behaviour GuildShardMapping

    @table_name :nostrum_guild_shard_map
    @record_name @table_name

    use Supervisor

    @doc "Retrieve the Mnesia table name used for the store."
    @spec table :: atom()
    def table, do: @table_name

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @doc "Drop the table used for the store."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(@table_name)

    @doc "Set up the store's Mnesia table."
    @impl Supervisor
    def init(_init_arg) do
      options = [
        attributes: [:guild_id, :shard_num],
        record_name: @record_name
      ]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @impl GuildShardMapping
    @doc "Create a new mapping for the given guild ID to the given shard ID."
    @spec create(Guild.id(), WSState.shard_num()) :: :ok
    def create(guild_id, shard_num) do
      :ok =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.write({@record_name, guild_id, shard_num})
        end)
    end

    @impl GuildShardMapping
    @doc "Delete any stored mapping for the given guild ID."
    @spec delete(Guild.id()) :: :ok
    def delete(guild_id) do
      :ok =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.delete({@table_name, guild_id})
        end)
    end

    @impl GuildShardMapping
    @doc "Get the shard number for the given guild ID."
    @spec get(Guild.id()) :: WSState.shard_num() | nil
    def get(guild_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, guild_id) do
          [{_tag, _guild_id, shard_num}] -> shard_num
          [] -> nil
        end
      end)
    end
  end
end
