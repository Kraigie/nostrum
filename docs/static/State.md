# State
Nostrum keeps track of the state that your bot can see. This state is updated
based on events from the WS connection. Most of the cache modules support only
simple interactions with the cache. Feel free to suggest additional functionality.

## Structs
Nostrum uses structs when appropriate to pass around objects from Discord.

In some cases, the struct modules will include helper functions for interacting
with the struct. See `Nostrum.Struct.Emoji.format_custom_emoji/2` for an example.

## Guilds
Each guild is ran in its own `GenServer` process, all of which are ran under a
supervisor. Behind the scenes, Nostrum uses the `Registry` module to
map guild ids to a `pid` to allow for lookup.

Please see `Nostrum.Cache.GuildCache` for more information on interacting with
guilds.

## Channels
DM channels are all stored in a single map in a single `GenServer`. Guild channels
are stored in their respective guilds.

Channels aren't important enough to have their own process per channel, and the
current implementation is not done well, in the future this will likely be moved
to an ETS backed cache like users are, but the interface will remain the same.

Please see `Nostrum.Cache.ChannelCache` for more information on interacting with
channels. Though they're stored in different places, the interface hides that fact.

## Users
Users are all stored in an ETS table, keyed off of their id.

To see the name of the ETS table associated with the user cache, and examples of
interacting with it both directly and through the provided abstractions, please
see `Nostrum.Cache.UserCache`.

## Mappings
There are some use cases where given a `channel_id` or `guild_id` we may want to
find the `guild` or `shard` that the `id` belongs to. For this purpose Nostrum
stores these types of relations in ETS tables.

The following mappings exists.
```Elixir
guild_channel_id -> guild_id
guild_id -> shard_num
shard_num_num -> shard_pid
```

Please see `Nostrum.Cache.Mapping.ChannelGuild` for one such map.
