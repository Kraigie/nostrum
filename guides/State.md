# State

Nostrum keeps track of the state that your bot can see. This state is updated
based on events from the WS connection. We differentiate between _caches_, which
are optional and are used to provide your bot with fresh data, and _state_,
which is mandatory state that we must track internally.

## Pluggable caching

The default ETS-based caches supplied by nostrum should work for most of your
needs, but all of the caches can be exchanged for your own implementations. For
this, implement the behaviours exported by the cache modules under
`Nostrum.Cache`.

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

### Implementations

#### ETS caching

Caching based on `:ets` is used by default. No configuration is required. Fast,
light on memory, but does not support any form of distribution or secondary
indexing: queries such as fetching all guild members for a guild by its ID will
perform a full table scan. For smaller bots, this is perfectly acceptable.


#### Mnesia caching

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
Guide](https://www.erlang.org/doc/apps/mnesia/users_guide.html) is a good
starting point.


#### NoOp caching

The NoOp cache adapters are supplied for the case where you do not want to cache
specific data from Discord at all.

These cache adapters presently also don't send out any data they receive either:
this means that for caches using the NoOp cache adapters, you won't receive any
gateway events.


### Cache invalidation

nostrum does not invalidate cache in any special way: it will maintain it in
response to gateway events (for instance by deleting a guild and its members
upon leaving it), but won't regularly prune caches or associate expiration times
with entries. For volatile (RAM-based) caches this is perfectly fine, however,
when implementing your own cache backend that persists to disk in some way, you
need to take care of this yourself.


### Cache performance

nostrum strives to provide the most performant caches on the Discord bot caching
market. If you run into performance issues with caches that you feel are not
adequately documented as such, please feel free to open an issue.

Benchmarks for caches can be found in the [`benchmarks/`
directory](https://github.com/Kraigie/nostrum/tree/master/benchmarks) of the
source code tree. If you want to get a feeling for how the caches perform or
implement optimizations, check them out.


## Query list comprehensions

The caches makes extensive use of Erlang's excellent
[qlc](https://www.erlang.org/doc/man/qlc.html) module. By [implementing a QLC
table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table), all
read operations from nostrum will be performed over your QLC table
implementation alone, and nostrum's dispatcher modules can easily be expanded
for more queries in the future. If you've never heard of QLC before, the
[`beam-lazy` repository](https://github.com/savonarola/beam-lazy) contains a
good introduction.

As an example, `Nostrum.Cache.MemberCache.fold_with_users/3` performs a join
between the member and the user cache without the caches knowing their exact API
(outside of the tuples yielded from the query handle). This is implemented via
the following QLC query:

```erl
get_with_users(RequestedGuildId, MemberCache, UserCache) ->
    qlc:q([{Member, User} || {{GuildId, MemberId}, Member} <- MemberCache:query_handle(),
                          GuildId =:= RequestedGuildId,
                          {UserId, User} <- UserCache:query_handle(),
                          MemberId =:= UserId]).
```

Using QLC bring a plethora of benefits. Implementation of a QLC table is
relatively simple, and gives us compile-time query optimization and compilation
in native Erlang list comprehension syntax. Furthermore, should you wish to
perform queries on your caches beyond what nostrum offers out of the box, you
can write your queries using the `query_handle/0` functions on our caches,
without having to investigate their exact API.

There is one caveat to be aware of when writing cache adapters in Elixir that
build on this functionality: While Erlang's QLC can perform intelligent query
optimization, a lot of it is implemented via a parse transform and thus only
available at compile time in Erlang modules. It is therefore recommended to
write your QLC queries in Erlang modules: in Mix projects this can be achieved
easily via the `src/` directory. Read the [QLC module
documentation](https://www.erlang.org/doc/man/qlc.html) for more details on the
optimizations done.


## Internal state

In addition to the optional caching, nostrum also needs to keep track of
internal state so it functions properly. State follows the same pattern as the
pluggable caching functionality described above, but disabling state storage via
`NoOp` as with caching is not possible.

The modules under `Nostrum.Store` are used for this functionality.



<!-- vim: set textwidth=80 sw=2 ts=2: -->
