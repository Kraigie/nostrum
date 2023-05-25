if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Store.RatelimitBucket.Mnesia do
    @moduledoc """
    Stores ratelimit buckets using Mnesia.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    alias Nostrum.Store.RatelimitBucket
    alias Nostrum.Util

    @behaviour RatelimitBucket

    @table_name :nostrum_ratelimit_buckets
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
        attributes: [:route, :remaining, :reset_time, :latency],
        record_name: @record_name
      ]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @impl RatelimitBucket
    @doc "Update an existing routes remaining calls."
    @spec update(RatelimitBucket.route(), RatelimitBucket.remaining()) :: :ok
    def update(route, remaining) do
      :ok =
        :mnesia.activity(:sync_transaction, fn ->
          case :mnesia.read(@table_name, route, :write) do
            [result] ->
              :mnesia.write(put_elem(result, 2, remaining))

            [] ->
              :ok
          end
        end)
    end

    @impl RatelimitBucket
    @doc "Set up a new ratelimiter bucket from the given arguments."
    @spec update(
            RatelimitBucket.route(),
            RatelimitBucket.remaining(),
            RatelimitBucket.reset_time(),
            RatelimitBucket.latency()
          ) :: :ok
    def update(route, remaining, reset_time, latency) do
      :ok =
        :mnesia.activity(:sync_transaction, fn ->
          :mnesia.write({@record_name, route, remaining, reset_time, latency})
        end)
    end

    @impl RatelimitBucket
    @doc "Look up the most relevant ratelimiter bucket for the given route."
    @spec lookup(RatelimitBucket.route()) :: RatelimitBucket.bucket() | nil
    def lookup(route) do
      {route_time, global_time} =
        :mnesia.activity(:sync_transaction, fn ->
          {:mnesia.read(@table_name, route), :mnesia.read(@table_name, "GLOBAL")}
        end)

      max =
        Enum.max_by([route_time, global_time], fn info ->
          case info do
            [] -> -1
            [{_tag, _route, _remaining, reset_time, _latency}] -> reset_time
          end
        end)

      case max do
        [] -> nil
        [{_tag, route, remaining, reset_time, latency}] -> {route, remaining, reset_time, latency}
      end
    end

    @impl RatelimitBucket
    @doc "Clean up bucket entries older than `age` milliseconds."
    @spec cleanup(pos_integer()) :: non_neg_integer()
    def cleanup(age) do
      then = Util.now() - age

      match_spec = [{{:_, :"$1", :_, :"$2", :_}, [{:<, :"$2", then}], ["$1"]}]

      :mnesia.activity(:sync_transaction, fn ->
        do_delete(:mnesia.select(@table_name, match_spec, 100, :write))
      end)
    end

    defp do_delete(starter) do
      do_delete(0, starter)
    end

    defp do_delete(deleted, {[route | matches], cont}) do
      :mnesia.delete(@table_name, route, :write)
      do_delete(deleted + 1, {matches, cont})
    end

    defp do_delete(deleted, {[], cont}) do
      do_delete(deleted, :mnesia.select(cont))
    end

    defp do_delete(deleted, :"$end_of_table") do
      deleted
    end
  end
end
