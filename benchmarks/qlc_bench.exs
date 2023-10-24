## qlc_bench
#
## PURPOSE
#
# This module benchmarks various ways of dealing with QLC tables. It is mainly
# aimed to gather performance insights about how to perform QLC queries in
# nostrum, and aims to potentially also propose improvements upstream, if
# applicable.
#
# Note that if using the QLC cursor functionality, memory measurements are not
# representative! QLC's cursor sets up a new process that is queried, and
# Benchee cannot measure that.
#
## SUPPORTING FILES
#
# This module requires that you put the following module into
# `src/qlctest.erl`: <<EOF
#
# -module(qlctest).
# -export([qh0/2, qh1b/2, qh2a/2]).
# 
# -include_lib("stdlib/include/qlc.hrl").
# 
# 
# qh0(Tab, RequestedId) ->
#   qlc:q([{Id, Id, Value} || {Id, Value} <- Tab, Id =:= RequestedId]).
# 
# qh1b(Tab, RequestedId) ->
#   V1 = qlc:q([{Id, Id, Value} || {Id, Value} <- Tab]),
#   qlc:q([Value || {Id, _, Value} <- V1, Id =:= RequestedId]).
# 
# qh2a(Tab, RequestedId) ->
#   qlc:q([{Id, Id, Value} || {Id, _, Value} <- Tab, Id =:= RequestedId]).
#
# EOF
# 
## FINDINGS
#
# Due to (presumably) the QLC parse transform compile time query optimizations,
# query list comprehensions written directly in Erlang can achieve better
# resource usage than those written in Elixir.

tab = :ets.new(:test, [:set, :public])
:ets.insert(tab, Enum.map(1..100_000, &{&1, %{id: &1, name: "#{&1}"}}))

IO.puts(
  "Table size is: #{:ets.info(tab, :memory) * :erlang.system_info(:wordsize) / 1024 / 1024} MB"
)

qh = :ets.table(tab)
ms = [{{:"$1", :"$2"}, [], [{{:"$1", :"$1", :"$2"}}]}]
qhtraverse = :ets.table(tab, traverse: {:select, ms})

# Elixir queries
qh0 =
  :qlc.string_to_handle(~c"[{Id, Id, Value} || {Id, Value} <- Handle, Id =:= RequestedId].", [],
    Handle: qh,
    RequestedId: 500_000
  )

qh1a =
  :qlc.string_to_handle(~c"[{Id, Id, Value} || {Id, Value} <- Handle].", [],
    Handle: qh,
    RequestedId: 500_000
  )

qh1b =
  :qlc.string_to_handle(~c"[Value || {Id, _, Value} <- Handle, Id =:= RequestedId].", [],
    Handle: qh1a,
    RequestedId: 500_000
  )

qh2a =
  :qlc.string_to_handle(~c"[{Id, Id, Value} || {Id, Value} <- Handle, Id =:= RequestedId].", [],
    Handle: qhtraverse,
    RequestedId: 500_000
  )

# Erlang queries
nqh0 = :qlctest.qh0(qh, 500_000)
nqh1b = :qlctest.qh1b(qh, 500_000)
nqh2a = :qlctest.qh2a(qhtraverse, 500_000)

# Print query information.
# These _should_ be the same between Elixir and Erlang.
# IO.puts("===== NAIVE (Elixir) =====")
# IO.puts(:qlc.info(qh0))
# IO.puts("===== NAIVE (Erlang) =====")
# IO.puts(:qlc.info(nqh0))
# IO.puts("===== NESTED (Elixir) =====")
# IO.puts(:qlc.info(qh1b))
# IO.puts("===== NESTED (Erlang) =====")
# IO.puts(:qlc.info(nqh1b))
# IO.puts("===== NAIVE, TRAVERSED (Elixir) =====")
# IO.puts(:qlc.info(qh2a))
# IO.puts("===== NAIVE, TRAVERSED (Erlang) =====")
# IO.puts(:qlc.info(nqh2a))

counter = fn _item, acc -> acc + 1 end

_cursor_counter = fn qh ->
  Stream.resource(
    fn -> :qlc.cursor(qh) end,
    fn cursor ->
      # With the default of 10, this takes forever.
      case :qlc.next_answers(cursor, 500) do
        results -> {[Enum.count(results)], cursor}
        [] -> {:halt, cursor}
      end
    end,
    fn cursor -> :qlc.delete_cursor(cursor) end
  )
end

must! = fn
  [] -> :ok
  [_] -> :ok
  num when is_integer(num) -> :ok
end

Benchee.run(
  %{
    # Queries coming from `string_to_handle` in Elixir.
    "naive" => fn -> must!.(:qlc.e(qh0)) end,
    "nested" => fn -> must!.(:qlc.e(qh1b)) end,
    "naive, traversed" => fn -> must!.(:qlc.e(qh2a)) end,
    "naive, fold" => fn -> must!.(:qlc.fold(counter, 0, qh0)) end,
    "nested, fold" => fn -> must!.(:qlc.fold(counter, 0, qh1b)) end,
    "naive, traversed, fold" => fn -> must!.(:qlc.fold(counter, 0, qh2a)) end,

    # No effect.
    # "naive, fold, uncached" => fn -> :qlc.fold(counter, 0, qh0, cache_all: false) end,
    # "nested, fold, uncached" => fn -> :qlc.fold(counter, 0, qh1b, cache_all: false) end,
    # "naive, traversed, fold, uncached" => fn -> :qlc.fold(counter, 0, qh2a, cache_all: false) end,

    # A lot slower.
    # "naive, cursor" => fn -> cursor_counter.(qh0) |> Enum.sum() end,
    # "nested, cursor" => fn -> cursor_counter.(qh1b) |> Enum.sum() end,
    # "naive, traversed, cursor" => fn -> cursor_counter.(qh2a) |> Enum.sum() end,

    # Queries coming from the parse transform in Erlang.
    "native, naive" => fn -> must!.(:qlc.e(nqh0)) end,
    "native, nested" => fn -> must!.(:qlc.e(nqh1b)) end,
    "native, naive, traversed" => fn -> must!.(:qlc.e(nqh2a)) end,
    "native, naive, fold" => fn -> must!.(:qlc.fold(counter, 0, nqh0)) end,
    "native, nested, fold" => fn -> must!.(:qlc.fold(counter, 0, nqh1b)) end,
    "native, naive, traversed, fold" => fn -> must!.(:qlc.fold(counter, 0, nqh2a)) end

    # No effect.
    # "native, naive, fold, uncached" => fn -> :qlc.fold(counter, 0, nqh0, cache_all: false) end,
    # "native, nested, fold, uncached" => fn -> :qlc.fold(counter, 0, nqh1b, cache_all: false) end,
    # "native, naive, traversed, fold, uncached" => fn -> :qlc.fold(counter, 0, nqh2a, cache_all: false) end,

    # A lot slower.
    # "native, naive, cursor" => fn -> cursor_counter.(nqh0) |> Enum.sum() end,
    # "native, nested, cursor" => fn -> cursor_counter.(nqh1b) |> Enum.sum() end,
    # "native, naive, traversed, cursor" => fn -> cursor_counter.(nqh2a) |> Enum.sum() end
  },
  memory_time: 2,
  time: 2
)
