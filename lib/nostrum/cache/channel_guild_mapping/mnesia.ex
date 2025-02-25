if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.ChannelGuildMapping.Mnesia do
    @moduledoc """
    An Mnesia-based mapping between channel and guild IDs.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    @base_table_name :nostrum_channel_guild_mapping

    alias Nostrum.Bot
    alias Nostrum.Cache.ChannelGuildMapping
    alias Nostrum.Struct.Channel
    alias Nostrum.Struct.Guild

    @behaviour ChannelGuildMapping

    use Supervisor

    @doc "Start the supervisor."
    def start_link(opts) do
      Supervisor.start_link(__MODULE__, opts)
    end

    @doc "Set up the ETS table."
    @impl Supervisor
    def init(opts) do
      table_name = :"#{@base_table_name}_#{Keyword.fetch!(opts, :name)}"

      table_options = [
        attributes: [:channel_id, :guild_id],
        record_name: table_name
      ]

      case :mnesia.create_table(table_name, table_options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @doc "Retrieve the table used by this module."
    @spec table() :: atom()
    def table, do: :"#{@base_table_name}_#{Bot.fetch_bot_name()}"

    defp record_name, do: table()

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(table())

    @impl ChannelGuildMapping
    @doc "Create a mapping of the given channel to the given guild."
    @spec create(Channel.id(), Guild.id()) :: true
    def create(channel_id, guild_id) do
      record = {record_name(), channel_id, guild_id}
      {:atomic, :ok} = :mnesia.sync_transaction(fn -> :mnesia.write(record) end)
      true
    end

    @impl ChannelGuildMapping
    @doc "Retrieve the guild ID for the given channel ID, if present."
    @spec get(Channel.id()) :: Guild.id() | nil
    def get(channel_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(table(), channel_id) do
          [{_tag, _channel_id, guild_id}] -> guild_id
          [] -> nil
        end
      end)
    end

    @impl ChannelGuildMapping
    @doc "Remove any mapping associated with the given channel."
    @spec delete(Channel.id()) :: true
    def delete(channel_id) do
      :mnesia.activity(:sync_transaction, fn ->
        :mnesia.delete(table(), channel_id, :write)
      end)

      true
    end
  end
end
