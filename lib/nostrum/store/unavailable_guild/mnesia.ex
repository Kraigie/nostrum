if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Store.UnavailableGuild.Mnesia do
    @moduledoc """
    Keeps track of unavailable guilds.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    alias Nostrum.Store.UnavailableGuild
    alias Nostrum.Struct.Guild

    @behaviour UnavailableGuild

    @table_name :nostrum_unavailable_guilds
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
        attributes: [:guild_id, :sentinel],
        record_name: @record_name
      ]

      case :mnesia.create_table(@table_name, options) do
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
          :mnesia.write({@record_name, guild_id, true})
        end)
    end

    @impl UnavailableGuild
    @doc "Return whether the given guild is unavailable."
    @spec is?(Guild.id()) :: boolean()
    def is?(guild_id) do
      result =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.read({@table_name, guild_id})
        end)

      match?([_], result)
    end
  end
end
