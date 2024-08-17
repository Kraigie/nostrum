# Benchmarks Nostrum's GuildCache implementations.

alias Nostrum.Cache.GuildCache

:ok = :mnesia.start()

make_guild = fn id ->
  %{
    id: id,
    name: "Guild #{id}",
    icon: nil,
    splash: nil,
    owner_id: :erlang.unique_integer([:positive]),
    afk_channel_id: nil,
    afk_timeout: 300,
    verification_level: 1,
    default_message_notifications: 1,
    explicit_content_filter: 2,
    roles: %{
      id => %{
        id: :erlang.unique_integer([:positive]),
        name: "@everyone",
        color: 0,
        hoist: false,
        position: 0,
        permissions: 0,
        managed: false,
        mentionable: false,
        icon: nil,
        unicode_emoji: nil
      }
    },
    emojis: [
      %{
        id: :erlang.unique_integer([:positive]),
        name: "An Emoji",
        user: nil,
        require_colons: true,
        managed: false,
        animated: false,
        roles: []
      }
    ],
    joined_at: DateTime.utc_now(),
    large: false,
    unavailable: false,
    member_count: Enum.random(1..1000),
    voice_states: [],
    channels: %{
      id => %{
        id: id,
        type: 1,
        guild_id: id,
        position: 1,
        name: "Test Channel"
      }
    }
  }
end

ets_standard_before = fn input ->
  me = self()

  spawn(fn ->
    {:ok, pid} = GuildCache.ETS.start_link([])
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
  {:ok, pid} = GuildCache.Mnesia.start_link([])

  {input, pid}
end

mnesia_standard_after = fn {_input, pid} ->
  Supervisor.stop(pid)
  GuildCache.Mnesia.teardown()
end

ets_get_before = fn input ->
  {input, pid} = ets_standard_before.(input)

  Enum.each(1..input, fn guild_id ->
    GuildCache.ETS.create(make_guild.(guild_id))
  end)

  {input, pid}
end

mnesia_get_before = fn input ->
  {input, pid} = mnesia_standard_before.(input)

  Enum.each(1..input, fn guild_id ->
    GuildCache.Mnesia.create(make_guild.(guild_id))
  end)

  {input, pid}
end

# Nostrum-facing read
Benchee.run(
  %{
    "ETS get/1" => {
      fn {guild_id, _} ->
        GuildCache.ETS.get(guild_id)
      end,
      before_scenario: ets_get_before, after_scenario: ets_standard_after
    },
    "Mnesia get/1" => {
      fn {guild_id, _} ->
        GuildCache.Mnesia.get(guild_id)
      end,
      before_scenario: mnesia_get_before, after_scenario: mnesia_standard_after
    }
  },
  inputs: %{"1 guild" => 1, "1_000 guilds" => 1_000, "10_000 guilds" => 10_000},
  memory_time: 2
)

ets_create_before = fn input ->
  {input, pid} = ets_get_before.(input)

  guild = make_guild.(:erlang.unique_integer([:positive]))

  {input, pid, guild}
end

ets_create_after = fn {input, pid, _guild} ->
  ets_standard_after.({input, pid})
end

mnesia_create_before = fn input ->
  {input, pid} = mnesia_get_before.(input)

  guild = make_guild.(:erlang.unique_integer([:positive]))

  {input, pid, guild}
end

mnesia_create_after = fn {input, pid, _guild} ->
  mnesia_standard_after.({input, pid})
end

# Nostrum-facing insert
Benchee.run(
  %{
    "ETS create/1" => {
      fn {_guild_id, _, guild} ->
        GuildCache.ETS.create(guild)
      end,
      before_scenario: ets_create_before, after_scenario: ets_create_after
    },
    "Mnesia create/1" => {
      fn {_guild_id, _, guild} ->
        GuildCache.Mnesia.create(guild)
      end,
      before_scenario: mnesia_create_before, after_scenario: mnesia_create_after
    }
  },
  inputs: %{"1 guild" => 1, "1_000 guilds" => 1_000, "10_000 guilds" => 10_000},
  memory_time: 2
)

# Nostrum-facing update
Benchee.run(
  %{
    "ETS update/1" => {
      fn {_guild_id, pid, guild} ->
        # using a guild with an id of 1 to ensure it exists
        GuildCache.ETS.update(%{id: 1, afk_timeout: 600})
      end,
      before_scenario: ets_create_before, after_scenario: ets_create_after
    },
    "Mnesia update/1" => {
      fn {_guild_id, pid, guild} ->
        GuildCache.Mnesia.update(%{id: 1, afk_timeout: 600})
      end,
      before_scenario: mnesia_create_before, after_scenario: mnesia_create_after
    }
  },
  inputs: %{"1 guild" => 1, "1_000 guilds" => 1_000, "10_000 guilds" => 10_000},
  memory_time: 2
)

# Nostrum-facing delete
Benchee.run(
  %{
    "ETS delete/1 Existing" => {
      fn {_guild_id, pid, guild} ->
        # using a guild with an id of 1 to ensure it exists
        GuildCache.ETS.delete(1)
      end,
      before_scenario: ets_create_before, after_scenario: ets_create_after
    },
    "ETS delete/1 Non-Existing" => {
      fn {_guild_id, pid, guild} ->
        GuildCache.ETS.delete(50_000_000_000)
      end,
      before_scenario: ets_create_before, after_scenario: ets_create_after
    },
    "Mnesia delete/1 Existing" => {
      fn {_guild_id, pid, guild} ->
        GuildCache.Mnesia.delete(1)
      end,
      before_scenario: mnesia_create_before, after_scenario: mnesia_create_after
    },
    "Mnesia delete/1 Non-Existing" => {
      fn {_guild_id, pid, guild} ->
        GuildCache.Mnesia.delete(50_000_000_000)
      end,
      before_scenario: mnesia_create_before, after_scenario: mnesia_create_after
    }
  },
  inputs: %{"1 guild" => 1, "1_000 guilds" => 1_000, "10_000 guilds" => 10_000},
  memory_time: 2
)
