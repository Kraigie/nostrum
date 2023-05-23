defmodule Nostrum.Store.RatelimitBucketMetaTest do
  alias Nostrum.Store.RatelimitBucket
  use ExUnit.Case, async: true

  @store_modules [
    # Dispatcher
    RatelimitBucket,
    # Implementations
    RatelimitBucket.ETS,
    RatelimitBucket.Mnesia
  ]

  for store <- @store_modules do
    defmodule :"#{store}Test" do
      use ExUnit.Case
      @store store
      doctest @store

      setup do
        if function_exported?(@store, :teardown, 0) do
          # Silence bogus warning with `apply`
          on_exit(:teardown, fn -> apply(@store, :teardown, []) end)
        end

        [pid: start_supervised!(@store)]
      end

      describe "empty lookup" do
        test "returns nothing" do
          refute @store.lookup("route")
        end

        test "succeeds in cleanup" do
          assert 0 = @store.cleanup(0)
        end

        test "allows update for new bucket" do
          reset_time = :erlang.unique_integer([:positive])
          assert @store.update("ROUTE", 3, reset_time, 123)
        end
      end

      describe "updating existing bucket" do
        @route "TESTBUCKET"

        setup context do
          reset_time = :erlang.unique_integer([:positive])
          remaining_calls = 3
          latency = 123
          @store.update(@route, remaining_calls, reset_time, latency)

          Map.merge(context, %{
            remaining: remaining_calls,
            reset_time: reset_time,
            latency: latency
          })
        end

        test "returns bucket", %{remaining: remaining, reset_time: reset_time, latency: latency} do
          assert {@route, ^remaining, ^reset_time, ^latency} = @store.lookup(@route)
        end

        test "timeout_for/1" do
          assert RatelimitBucket.timeout_for(@route, @store)
        end
      end
    end
  end
end
