# Pluggable caching

The default ETS-based caches supplied by nostrum should work for most of your
needs, but all of the caches can be exchanged for your own implementations. For
this, implement the behaviours exported by the cache modules under
`Nostrum.Cache`.

> ### Exception {: .info}
>
> The exception to the above is the `Nostrum.Cache.MessageCache`, which does not
> include an ETS-based implementation, and defaults to a NoOp cache. This is
> an intentional design decision because caching messages consumes a
> lot more memory than other objects, and is often not needed by most users.

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

In addition to regular caches that associate Discord snowflakes with the proper
"full" object, nostrum also maintains junction table-like mappings that allow
you to find the matching object from one cache in another. One example for this
is `Nostrum.Cache.ChannelGuildMapping`.

Nostrum also ships with Mnesia-based caches. **These are only compiled in when
mnesia is available**: they may not be available on Nerves or when Mnesia was
not installed with OTP.

## Implementations

### ETS caching

Caching based on `:ets` is used by default. No configuration is required. Fast,
light on memory, but does not support any form of distribution or secondary
indexing: queries such as fetching all guild members for a guild by its ID will
perform a full table scan. For smaller bots, this is perfectly acceptable.


### Mnesia caching

Mnesia-based caching is mainly suggested for larger bots that require
features such as cache distribution, fragmentation, secondary indexing and more.

The caches will attempt to create their tables automatically at startup:
therefore, Mnesia must be started ahead of nostrum. Caches expose a function
`table/0` that can be called to retrieve the table name used by the cache and
perform schema operations on it, such as adding replicas or fragmenting them.

<!-- From 1.0, add the following: Any future schema migrations that nostrum
needs to perform will be automatically performed at cache startup. -->

Access to Mnesia is presently done in `sync_transaction` mode for best
consistency. If needed, a compile-time configuration option for the cache to
switch this can be added.

Mnesia-based caching assumes the user is familar with usage and
maintenance of Mnesia: the [Mnesia User's
Guide](https://www.erlang.org/doc/apps/mnesia/mnesia_chap1.html) is a good
starting point.


### NoOp caching

The NoOp cache adapters are supplied for the case where you do not want to cache
specific data from Discord at all.

## Cache invalidation

Nostrum does not invalidate most caches in any special way: it will maintain it in
response to gateway events (for instance by deleting a guild and its members
upon leaving it), but won't regularly prune caches or associate expiration times
with entries. For volatile (RAM-based) caches this is perfectly fine, however,
when implementing your own cache backend that persists to disk in some way, you
need to take care of this yourself.

The exception to this is the `Nostrum.Cache.MessageCache.Mnesia` module, which has a
default size limit of 10,000 and will automatically remove the 100 oldest
messages when this limit is reached as well as delete all cached messages for a
channel when the channel is deleted.


## Cache performance

nostrum strives to provide the most performant caches on the Discord bot caching
market. If you run into performance issues with caches that you feel are not
adequately documented as such, please feel free to open an issue.

Benchmarks for caches can be found in the [`benchmarks/`
directory](https://github.com/Kraigie/nostrum/tree/master/benchmarks) of the
source code tree. If you want to get a feeling for how the caches perform or
implement optimizations, check them out.
