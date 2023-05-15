defmodule Nostrum.Store.RatelimitBucket.ETS do
  @moduledoc """
  Stores ratelimit buckets via `:ets`.

  If programmatic access to the ETS table is needed, please use the `table/0`
  function.

  Please do not use this module directly, apart from special functions such as
  `table/0`. Use `Nostrum.Store.RatelimitBucket` to call the configured
  mapping instead.
  """
  @moduledoc since: "0.8.0"

  alias Nostrum.Store.RatelimitBucket
  alias Nostrum.Util
  use Supervisor

  @behaviour RatelimitBucket

  @table_name :nostrum_ratelimit_buckets

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
    :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl RatelimitBucket
  @doc "Update an existing routes remaining calls."
  @spec update(RatelimitBucket.route(), RatelimitBucket.remaining()) :: boolean()
  def update(route, remaining) do
    :ets.update_element(@table_name, route, {2, remaining})
  end

  @impl RatelimitBucket
  @doc "Set up a new ratelimiter bucket from the given arguments."
  @spec update(
          RatelimitBucket.route(),
          RatelimitBucket.remaining(),
          RatelimitBucket.reset_time(),
          RatelimitBucket.latency()
        ) :: true
  def update(route, remaining, reset_time, latency) do
    :ets.insert(@table_name, {route, remaining, reset_time, latency})
  end

  @impl RatelimitBucket
  @doc "Look up the most relevant ratelimiter bucket for the given route."
  @spec lookup(RatelimitBucket.route()) :: RatelimitBucket.bucket() | nil
  def lookup(route) do
    route_time = :ets.lookup(@table_name, route)
    global_time = :ets.lookup(@table_name, "GLOBAL")

    max =
      Enum.max_by([route_time, global_time], fn info ->
        case info do
          [] -> -1
          [{_route, _remaining, reset_time, _latency}] -> reset_time
        end
      end)

    case max do
      [] -> nil
      [entry] -> entry
    end
  end

  @impl RatelimitBucket
  @doc "Clean up bucket entries older than `age` milliseconds."
  @spec cleanup(pos_integer()) :: non_neg_integer()
  def cleanup(age) do
    then = Util.now() - age

    # created from :ets.fun2ms(
    # fn {_, _, reset_time, _} when reset_time < then -> true end
    # )
    match_spec = [{{:_, :_, :"$1", :_}, [{:<, :"$1", then}], [true]}]
    :ets.select_delete(@table_name, match_spec)
  end
end
