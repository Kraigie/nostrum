# State
Mixcord provides a number of GenServers to maintain state.
These tables are updated based on events from the WS connection.

See any of the Cache modules for default methods provided by the lib; e.g.
`Guild` information can be found at `Mixcord.Cache.Guild`

## Structs
Mixcord uses structs when appropriate to pass around objects from Discord. In some
instances, such as the cache and dispatch event handles, Discord's objects will be
represented as maps. It will be made clear when this is the case using `@spec`
specifications.

In some cases, the struct modules will include helper functions for interacting
with the struct. See `Mixcord.Struct.Emoji.format_custom_emoji/2` for an example.
