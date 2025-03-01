if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Store.UnavailableGuild.Mnesia do
    @moduledoc """
    Keeps track of unavailable guilds.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    alias Nostrum.Bot
    alias Nostrum.Store.UnavailableGuild
    alias Nostrum.Struct.Guild

    @behaviour UnavailableGuild

    @base_table_name :nostrum_unavailable_guilds

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
        attributes: [:guild_id, :sentinel],
        record_name: table_name
      ]

      case :mnesia.create_table(table_name, table_options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @impl UnavailableGuild
    @doc "Create the given guild as an unavailable guild."
    @spec create(Guild.id()) :: :ok
    def create(guild_id) do
      :ok =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.write({record_name(), guild_id, true})
        end)
    end

    @impl UnavailableGuild
    @doc "Return whether the given guild is unavailable."
    @spec is?(Guild.id()) :: boolean()
    def is?(guild_id) do
      result =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.read({table(), guild_id})
        end)

      match?([_], result)
    end
  end
end
