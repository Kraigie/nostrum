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
bot across multiple nodes, please see the [pluggable
caching](../advanced/pluggable_caching.md) documentation.


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
