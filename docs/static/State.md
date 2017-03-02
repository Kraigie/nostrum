# State
Mixcord keeps track of the state that your bot can see. This state is updated
based on events from the WS connection. Most of the cache modules support only
simple interactions with the cache. Feel free to suggest additional functionality.

## Structs
Mixcord uses structs when appropriate to pass around objects from Discord.

In some cases, the struct modules will include helper functions for interacting
with the struct. See `Mixcord.Struct.Emoji.format_custom_emoji/2` for an example.

## Guilds
Each guild is ran in its own `GenServer` process, all of which are ran under a
supervisor. Behind the scenes, Mixcord uses the `Registry` module to
map guild ids to a `pid` to allow for lookup.

Please see `Mixcord.Cache.Guild.GuildServer` for more information on interacting with
guilds.

## Channels
DM channels are all stored in a single map in a single `GenServer`. Guild channels
are stored in their respective guilds.

Channels aren't important enough to have their own process per channel, and the
current implementation is not done well, in the future this will likely be moved
to an ETS backed cache like users are, but the interface will remain the same.

Please see `Mixcord.Cache.ChannelCache` for more information on interacting with
channels. Though they're stored in different places, the interface hides that fact.

## Users
Users are all stored in an ETS table, keyed off of their id.

To see the name of the ETS table associated with the user cache, and examples of
interacting with it both directly and through the provided abstractions, please
see `Mixcord.Cache.UserCache`.
