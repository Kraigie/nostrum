# Benchmarks Nostrum's MemberCache implementations.

alias Nostrum.Cache.MemberCache

:ok = :mnesia.start()

ets_standard_before = fn input ->
  me = self()

  spawn(fn ->
    {:ok, pid} = MemberCache.ETS.start_link([])
    send(me, pid)

    receive do
      :nothing -> :successfully
    after
      :timer.minutes(5) -> :ok
    end
  end)

  pid =
    receive do
      it -> it
    end

  {input, pid}
end

ets_standard_after = fn {_input, pid} ->
  :ok = Supervisor.stop(pid)
end

mnesia_standard_before = fn input ->
  {:ok, pid} = MemberCache.Mnesia.start_link([])
  {input, pid}
end

mnesia_standard_after = fn {_input, pid} ->
  Supervisor.stop(pid)
  MemberCache.Mnesia.teardown()
end

# Nostrum-facing bulk insert
Benchee.run(
  %{
    "ETS bulk_create/2" => {
      fn {{guild_id, member_chunk}, _} ->
        MemberCache.ETS.bulk_create(guild_id, member_chunk)
      end,
      before_scenario: ets_standard_before, after_scenario: ets_standard_after
    },
    "Mnesia bulk_create/2" => {
      fn {{guild_id, member_chunk}, _} ->
        MemberCache.Mnesia.bulk_create(guild_id, member_chunk)
      end,
      before_scenario: mnesia_standard_before, after_scenario: mnesia_standard_after
    }
  },
  inputs: %{"1_000 members" => 1_000},
  before_scenario: fn input ->
    guild_id = :erlang.unique_integer([:positive])
    member_chunk = Enum.map(1..input, &%{user: %{id: &1}})
    {guild_id, member_chunk}
  end,
  memory_time: 2
)

ets_insert_before = fn {guild_id, _, member_chunks} = input ->
  result = ets_standard_before.(input)
  Enum.map(member_chunks, &MemberCache.ETS.bulk_create(guild_id, &1))
  result
end

ets_insert_scenario = fn fun ->
  {fun, before_scenario: ets_insert_before, after_scenario: ets_standard_after}
end

mnesia_insert_before = fn {guild_id, _, member_chunks} = input ->
  result = mnesia_standard_before.(input)
  Enum.map(member_chunks, &MemberCache.Mnesia.bulk_create(guild_id, &1))
  result
end

mnesia_insert_scenario = fn fun ->
  {fun, before_scenario: mnesia_insert_before, after_scenario: mnesia_standard_after}
end

# Nostrum-facing operations on existing data
Benchee.run(
  %{
    "ETS create/2" =>
      ets_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.ETS.create(guild_id, Map.put(member, :user, %{id: member.user.id * 4}))
      end),
    "ETS update/2 existing" =>
      ets_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.ETS.update(guild_id, member)
      end),
    "ETS update/2 nonexisting" =>
      ets_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.ETS.update(guild_id + 1, member)
      end),
    "ETS delete/2 existing" =>
      ets_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.ETS.delete(guild_id, member.user.id)
      end),
    "ETS delete/2 nonexisting" =>
      ets_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.ETS.delete(guild_id + 1, member.user.id)
      end),

    # Mnesia
    "Mnesia create/2" =>
      mnesia_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.Mnesia.create(guild_id, Map.put(member, :user, %{id: member.user.id * 4}))
      end),
    "Mnesia update/2 existing" =>
      mnesia_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.Mnesia.update(guild_id, member)
      end),
    "Mnesia update/2 nonexisting" =>
      mnesia_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.Mnesia.update(guild_id + 1, member)
      end),
    "Mnesia delete/2 existing" =>
      mnesia_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.Mnesia.delete(guild_id, member.user.id)
      end),
    "Mnesia delete/2 nonexisting" =>
      mnesia_insert_scenario.(fn {{guild_id, member, _}, _} ->
        MemberCache.Mnesia.delete(guild_id + 1, member.user.id)
      end)
  },
  inputs: %{
    "100_000 existing members" => 100_000,
    "1_000_000 existing members" => 1_000_000
  },
  before_scenario: fn input ->
    guild_id = :erlang.unique_integer([:positive])

    member_chunks =
      1..input
      |> Stream.map(&%{user: %{id: &1}})
      |> Stream.chunk_every(1_000)

    {guild_id, %{user: %{id: trunc(input / 2), roles: [guild_id]}}, member_chunks}
  end,
  memory_time: 2
)

must! = fn
  {:ok, _member} -> :ok
  [_ | _] -> :ok
  num when is_integer(num) and num > 0 -> :ok
end

mustnt! = fn
  {:error, :not_found} -> :ok
  [] -> :ok
  0 -> :ok
end

# User-facing lookups
Benchee.run(
  %{
    # ETS
    "ETS get/3 existing" =>
      ets_insert_scenario.(fn {{guild_id, member_id, _}, _} ->
        must!.(MemberCache.get(guild_id, member_id, MemberCache.ETS))
      end),
    "ETS get/3 nonexisting" =>
      ets_insert_scenario.(fn {{guild_id, member_id, _}, _} ->
        mustnt!.(MemberCache.get(guild_id + 1, member_id + 1, MemberCache.ETS))
      end),
    "ETS fold/4 existing" =>
      ets_insert_scenario.(fn {{guild_id, _member_id, _}, _} ->
        must!.(MemberCache.fold(0, guild_id, fn _member, acc -> acc + 1 end, MemberCache.ETS))
      end),
    "ETS fold/4 nonexisting" =>
      ets_insert_scenario.(fn {{guild_id, _member_id, _}, _} ->
        mustnt!.(
          MemberCache.fold(0, guild_id + 1, fn _member, acc -> acc + 1 end, MemberCache.ETS)
        )
      end),
    "ETS fold_by_user/4 existing" =>
      ets_insert_scenario.(fn {{_guild_id, member_id, _}, _} ->
        must!.(
          MemberCache.fold_by_user(
            0,
            member_id,
            fn _member, acc -> acc + 1 end,
            MemberCache.ETS
          )
        )
      end),
    "ETS fold_by_user/4 nonexisting" =>
      ets_insert_scenario.(fn {{_guild_id, member_id, _}, _} ->
        mustnt!.(
          MemberCache.fold_by_user(
            0,
            member_id * 4,
            fn _member, acc -> acc + 1 end,
            MemberCache.ETS
          )
        )
      end),

    # Mnesia
    "Mnesia get/3 existing" =>
      mnesia_insert_scenario.(fn {{guild_id, member_id, _}, _} ->
        must!.(MemberCache.get(guild_id, member_id, MemberCache.Mnesia))
      end),
    "Mnesia get/3 nonexisting" =>
      mnesia_insert_scenario.(fn {{guild_id, member_id, _}, _} ->
        mustnt!.(MemberCache.get(guild_id + 1, member_id + 1, MemberCache.Mnesia))
      end),
    "Mnesia fold/4 existing" =>
      mnesia_insert_scenario.(fn {{guild_id, _member_id, _}, _} ->
        must!.(MemberCache.fold(0, guild_id, fn _member, acc -> acc + 1 end, MemberCache.Mnesia))
      end),
    "Mnesia fold/4 nonexisting" =>
      mnesia_insert_scenario.(fn {{guild_id, _member_id, _}, _} ->
        mustnt!.(
          MemberCache.fold(0, guild_id + 1, fn _member, acc -> acc + 1 end, MemberCache.Mnesia)
        )
      end),
    "Mnesia fold_by_user/4 existing" =>
      mnesia_insert_scenario.(fn {{_guild_id, member_id, _}, _} ->
        must!.(
          MemberCache.fold_by_user(
            0,
            member_id,
            fn _member, acc -> acc + 1 end,
            MemberCache.Mnesia
          )
        )
      end),
    "Mnesia fold_by_user/4 nonexisting" =>
      mnesia_insert_scenario.(fn {{_guild_id, member_id, _}, _} ->
        mustnt!.(
          MemberCache.fold_by_user(
            0,
            member_id * 4,
            fn _member, acc -> acc + 1 end,
            MemberCache.Mnesia
          )
        )
      end)
  },
  inputs: %{
    "100_000 existing members" => 100_000,
    "1_000_000 existing members" => 1_000_000
  },
  before_scenario: fn input ->
    guild_id = :erlang.unique_integer([:positive])

    member_chunks =
      1..input
      |> Stream.map(&%{user: %{id: &1}})
      |> Stream.chunk_every(1_000)

    {guild_id, trunc(input / 2), member_chunks}
  end,
  memory_time: 2
)
