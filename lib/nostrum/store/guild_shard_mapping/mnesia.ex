if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Store.GuildShardMapping.Mnesia do
    @moduledoc """
    Maintains a mapping of guild IDs to their shard numbers using Mnesia.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    alias Nostrum.Bot
    alias Nostrum.Store.GuildShardMapping
    alias Nostrum.Struct.Guild
    alias Nostrum.Struct.WSState

    @behaviour GuildShardMapping

    @base_table_name :nostrum_guild_shard_map

    use Supervisor

    @doc "Retrieve the Mnesia table name used for the store."
    @spec table :: atom()
    def table, do: :"#{@base_table_name}_#{Bot.fetch_bot_name()}"

    defp record_name, do: table()

    @doc "Start the supervisor."
    def start_link(opts) do
      Supervisor.start_link(__MODULE__, opts)
    end

    @doc "Drop the table used for the store."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(table())

    @doc "Set up the store's Mnesia table."
    @impl Supervisor
    def init(opts) do
      table_name = :"#{@base_table_name}_#{Keyword.fetch!(opts, :name)}"

      table_options = [
        attributes: [:guild_id, :shard_num],
        record_name: table_name
      ]

      case :mnesia.create_table(table_name, table_options) do
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
          :mnesia.write({record_name(), guild_id, shard_num})
        end)
    end

    @impl GuildShardMapping
    @doc "Delete any stored mapping for the given guild ID."
    @spec delete(Guild.id()) :: :ok
    def delete(guild_id) do
      :ok =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.delete({table(), guild_id})
        end)
    end

    @impl GuildShardMapping
    @doc "Get the shard number for the given guild ID."
    @spec get(Guild.id()) :: WSState.shard_num() | nil
    def get(guild_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(table(), guild_id) do
          [{_tag, _guild_id, shard_num}] -> shard_num
          [] -> nil
        end
      end)
    end
  end
end
