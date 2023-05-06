# State
Nostrum keeps track of the state that your bot can see. This state is updated
based on events from the WS connection. Most of the cache modules support only
simple interactions with the cache. Feel free to suggest additional functionality.

## Pluggable caches

The default caches supplied by nostrum should work for most of your needs, but
all of the caches can be exchanged for your own implementations. For this,
implement the behaviours exported by the cache modules under `Nostrum.Cache`.

Use the `[:nostrum, :caches]` configuration for configuring which cache
implementation you want to use. This can only be set at dependency compilation
time. A common situation is that you don't want to cache presences in your bot,
most likely you don't care about user's status, so you can disable it altogether
by using the `NoOp` presence cache:

```elixir
config :nostrum,
  caches: %{
    presences: Nostrum.Cache.PresenceCache.NoOp
  }
```

## Structs
Nostrum uses structs when appropriate to pass around objects from Discord.

In some cases, the struct modules will include helper functions for interacting
with the struct. See `Nostrum.Struct.Emoji.image_url/1` for an example.

## Guilds
By default, guilds are cached in an ETS table, keyed off their ID. You can
obtain the table name of the backing ETS table using the `tabname` function
exported by the `Nostrum.Cache.GuildCache.ETS` module.

If you do not have the `:guilds` intent enabled, it is recommended to use the `NoOp`
cache for guilds.

Please see `Nostrum.Cache.GuildCache` for more information on interacting with
guilds.

## Channels
By default, DM channels are stored in an ETS table, see
`Nostrum.Cache.ChannelCache.ETS`. Guild channels are stored in their respective
guilds.

## Users
By default, users are all stored in an ETS table, keyed off of their id.

To see the name of the ETS table associated with the user cache, and examples of
interacting with it both directly and through the provided abstractions, please
see `Nostrum.Cache.UserCache.ETS`.

## Mappings
There are some use cases where given a `channel_id` or `guild_id` we may want to
find the `guild` or `shard` that the `id` belongs to. For this purpose Nostrum
stores these types of relations in ETS tables.

The following mappings exists.
```elixir
guild_channel_id -> guild_id
guild_id -> shard_num
```

Please see `Nostrum.Cache.Mapping.ChannelGuild` for one such map.
