# Benchmarks Nostrum's MemberCache implementations.

alias Nostrum.Cache.MemberCache

{:ok, _ets_pid} = MemberCache.ETS.start_link([])

# Nostrum-facing bulk insert
Benchee.run(
  %{
    "ETS bulk_create/2" => fn {guild_id, member_chunk} -> MemberCache.ETS.bulk_create(guild_id, member_chunk) end,
  },
  inputs: %{"1_000 members" => 1_000},
  before_scenario: fn input ->
    guild_id = :erlang.unique_integer([:positive])
    member_chunk = Enum.map(1..input, &%{user: %{id: &1}})
    {guild_id, member_chunk}
  end,
  after_scenario: fn _input ->
    :ets.delete_all_objects(MemberCache.ETS.table())
  end,
  memory_time: 2
)

# Nostrum-facing operations on existing data
Benchee.run(
  %{
    "ETS create/2" => fn {guild_id, member} -> MemberCache.ETS.create(guild_id, Map.put(member, :user, %{id: member.user.id * 4})) end,
    "ETS update/2 existing" => fn {guild_id, member} -> MemberCache.ETS.update(guild_id, member) end,
    "ETS update/2 nonexisting" => fn {guild_id, member} -> MemberCache.ETS.update(guild_id + 1, member) end,
    "ETS delete/2 existing" => fn {guild_id, member} -> MemberCache.ETS.get(guild_id, member.user.id) end,
    "ETS delete/2 nonexisting" => fn {guild_id, member} -> MemberCache.ETS.get(guild_id + 1, member.user.id) end,
  },
  inputs: %{
    "100_000 existing members" => 100_000,
    "1_000_000 existing members" => 1_000_000,
  },
  before_scenario: fn input ->
    guild_id = :erlang.unique_integer([:positive])
    1..input
    |> Stream.map(&%{user: %{id: &1}})
    |> Stream.chunk_every(1_000)
    |> Enum.map(&MemberCache.ETS.bulk_create(guild_id, &1))

    {guild_id, %{user: %{id: trunc(input / 2), roles: [guild_id]}}}
  end,
  after_scenario: fn _input ->
    :ets.delete_all_objects(MemberCache.ETS.table())
  end,
  memory_time: 2
)

# User-facing lookups
Benchee.run(
  %{
    "ETS get/1 existing" => fn {_guild_id, member_id} -> Enum.count(MemberCache.ETS.get(member_id)) end,
    "ETS get/1 nonexisting" => fn {_guild_id, member_id} -> Enum.count(MemberCache.ETS.get(member_id + 1)) end,
    "ETS get/2 existing" => fn {guild_id, member_id} -> MemberCache.ETS.get(guild_id, member_id) end,
    "ETS get/2 nonexisting" => fn {guild_id, member_id} -> MemberCache.ETS.get(guild_id + 1, member_id + 1) end,
    "ETS by_user/1 existing" => fn {_guild_id, member_id} -> Enum.count(MemberCache.ETS.by_user(member_id)) end,
    "ETS by_user/1 nonexisting" => fn {_guild_id, member_id} -> Enum.count(MemberCache.ETS.by_user(member_id + 1)) end,
  },
  inputs: %{
    "100_000 existing members" => 100_000,
    "1_000_000 existing members" => 1_000_000,
  },
  before_scenario: fn input ->
    guild_id = :erlang.unique_integer([:positive])
    1..input
    |> Stream.map(&%{user: %{id: &1}})
    |> Stream.chunk_every(1_000)
    |> Enum.map(&MemberCache.ETS.bulk_create(guild_id, &1))

    {guild_id, trunc(input / 2)}
  end,
  after_scenario: fn _input ->
    :ets.delete_all_objects(MemberCache.ETS.table())
  end,
  memory_time: 2
)
