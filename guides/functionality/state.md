# State

Nostrum keeps track of the state that your bot can see, which is updated based
on events from the WS connection. We differentiate between _caches_, which are
optional and are used to provide your bot with fresh data, and _state_, which is
mandatory state that we must track internally.

## Caches

Caching will by default use Erlang's ETS tables. Abstractions are provided for
common operations. If you feel the caches are missing some abstraction, feel
free to suggest it [on GitHub](https://github.com/Kraigie/nostrum/issues).

Should the default ETS-based caching not be enough for you - for instance, you
want to integrate to some external caching mechanism or want to distribute your
bot across multiple nodes, and the built-in Mnesia-based caching is not enough
for you either, please see the [pluggable
caching](../advanced/pluggable_caching.md) documentation.


## Implementing your own caches

To implement custom caches, implement the behaviour defined by the cache
module, such as `Nostrum.Cache.GuildCache`. For ease of use, these modules
define both a user-facing API to obtain objects from the configured cache, as
well as the developer-facing behaviour description.


## Internal state

In addition to the optional caching, nostrum also needs to keep track of
internal state so it functions properly. State follows the same pattern as the
pluggable caching functionality described above, but disabling state storage via
`NoOp` as with caching is not possible.

The modules under `Nostrum.Store` are used for this functionality.



<!-- vim: set textwidth=80 sw=2 ts=2: -->
