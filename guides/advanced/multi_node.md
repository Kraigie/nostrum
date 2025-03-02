# Multi-node

At the moment, you can run nostrum in highly available mode across multiple
nodes via OTP's distributed application support, and you can move bots between
nodes with no interruption in service.

As a general rule: if you are running distributed Erlang over the internet, make
sure to secure it with [a solid VPN](https://www.wireguard.com) and / or by
[using TLS for Erlang
distribution](https://www.erlang.org/doc/apps/ssl/ssl_distribution.html).


## High availability

Running using OTP's [distributed
applications](https://www.erlang.org/doc/design_principles/distributed_applications.html)
allows us to connect multiple nodes together and have your app and nostrum
rescheduled on another node when things go south. Let's see how we can configure
it. In this example, we will make use of three nodes, and all of them will be
run from your bot's directory. The only difference on their command line is the
`--sname` / `--name` you specify. We'll use `--sname`s for testing here, for
proper fault tolerance you will want to use multiple hosts with `--name`. Let's
assume we name our nodes `joe`, `robert`, and `mike`.


### Setting up distribution

The avid reader will probably know that starting with the same `--cookie` and
`--sname` / `--name` is only step one, the nodes need to connect to each other
as well.

To be able to test this in interactive mode we will configure the settings in
Erlang configuration files, for releases you can use your regular
`config/prod.exs`. We will set up the following:

- Instruct OTP that our app, `:mybot` is a distributed app, and give it the
  hosts to run it on.

- On startup, tell OTP it should wait for the other nodes to become available.

With the Erlang configuration files, this can be done as follows:

```erl
% mybot_joe.config
[{kernel,
  [{distributed, [{mybot, 5000, [joe@HOSTNAME, {mike@HOSTNAME, robert@HOSTNAME}]}]},
   {sync_nodes_mandatory, [mike@HOSTNAME, robert@HOSTNAME]},
   {sync_nodes_timeout, 30000}]}].
```
```erl
% mybot_robert.config
[{kernel,
  [{distributed, [{mybot, 5000, [joe@HOSTNAME, {mike@HOSTNAME, robert@HOSTNAME}]}]},
   {sync_nodes_mandatory, [joe@HOSTNAME, mike@HOSTNAME]},
   {sync_nodes_timeout, 30000}]}].
```
```erl
% mybot_mike.config
[{kernel,
  [{distributed, [{mybot, 5000, [joe@HOSTNAME, {mike@HOSTNAME, robert@HOSTNAME}]}]},
   {sync_nodes_mandatory, [joe@HOSTNAME, robert@HOSTNAME]},
   {sync_nodes_timeout, 30000}]}].
```

Note the only thing that changes is the `sync_node_mandatory` setting, which
instructs OTP which hosts to wait for on startup. The other settings must match.
These options instructs OTP that our app `:mybot` is distributed and should be
started at `:joe@HOSTNAME` first. If that fails, it moves to `:robert@HOSTNAME`
or `:mike@HOSTNAME`.

For details on the options, please see the [kernel reference
manual](https://www.erlang.org/doc/man/kernel_app.html).


### Playtest

In three distinct windows, run the following:

1. `iex --sname joe --cookie foo --erl-config myapp_joe.config -S mix`
2. `iex --sname robert --cookie foo --erl-config myapp_robert.config -S mix`
3. `iex --sname mike --cookie foo --erl-config myapp_mike.config -S mix`

If you have some other application that breaks on startup now - like monitoring
exporters that bind to specific ports, or similar things - this is when they
will blow up. Decide whether you want to run this on every node indeed or
include it with your app as shown above.

You now have three instances of the VM running. `:joe@HOSTNAME` runs your bot
right now. If you stop that node, one of the other two nodes will start running
your app. High availability complete.


### Being informed about takeover

Your application's `def start` function takes a `type` argument. In this case,
on the node that now runs your application, that `type` was `{:failover,
:joe@HOSTNAME}`. If you start `:joe@HOSTNAME` back up, `:joe@HOSTNAME` is
started with `{:takeover, source_node}`, where `source_node` is the node that it
took over from.


### Manual takeover

If you want to move your app around manually, you can use
`:application.takeover`, for example `:application.takeover(:mybot,
:permanent)`.


### Final thoughts

With this setup, nostrum can not perform any state synchronization between
nodes, it is an effective restart from scratch. For most bots, this type of
failover will be sufficient.


## Migrating bots between nodes

nostrum allows you to seamlessly migrate your bot from one to another node,
without losing any state or events. However, this requires some work.

### Setup

You will need the following prerequisites:

- You need to use mnesia-based caching _and_ mnesia-based stores, see [the
  pluggable caching docs](pluggable_caching.html). You may, of course, also use
  another external caching & store implementation which allows both nodes to work with
  the same data.

- At least the new bot needs to be started with manual sharding enabled by
  setting `shards: :manual` in the associated `t:Nostrum.Bot.bot_options/0?`.

Let's walk through this with a bot you're running locally to illustrate it.


### Walkthrough

We have configured our bot to use the mnesia-based caching, and set `shards:
:manual` in our bot options.

We will start our bot in one terminal window:

```sh
iex --cookie mybot --sname joe -S mix
```

Let's start the shard session too:

```elixir
iex(joe@host)1> Nostrum.Shard.Supervisor.connect(0, 1, MyBot.bot_options())
```

For the purpose of this example, `mike` is the node we want our bot to migrate
away from. In another terminal window, we will start it using:

```sh
iex --cookie mybot --sname mike -S mix run --no-start
```

We then connect `mike` to `joe` (note that `@host` depends on your hostname):

```elixir
iex(mike@host)1> Node.connect(:joe@host)
true
```

We will now connect `mike` to `joe`'s mnesia schema and persist it on mike:

```elixir
iex(mike@host)2> :mnesia.start(extra_db_nodes: [:joe@host])
:ok
iex(mike@host)3> :mnesia.change_table_copy_type(:schema, node(), :disc_copies)
{:atomic, :ok}
```

Running `:mnesia.info/0` reveals that mnesia considers both `joe@host` and
`mike@host` part of the cluster, whilst the `remote` tables are sitting on
`joe@host`. Let's start the bot locally now, by first stopping the current
session cleanly.

```elixir
iex(mike@host)4> :init.stop()
:ok
```

This should stop your node. Now start it again using:

```sh
iex --cookie mybot --sname mike -S mix
```

This time, the bot will start as well. Note that due to `:mnesia` starting and
finding `joe@host` as part of its cluster in the schema, the other node will
auto-connect:

```elixir
iex(mike@host)1> Node.list
[:joe@host]
```

Let's start by migrating our shard session over from `joe@host` to `mike@host`.
To do this, we will disconnect it from `joe@host`, transfer the resume
information to `mike@host`, and then start the shard on `mike@host`.

We will register our shell process on `mike@host` first:
```elixir
iex(mike@host)2> :global.register_name(:target, self())
:yes
```

Then we will disconnect the shard on `joe@host` and send the resume information
to `:target` after informing nostrum which bot we want to disconnect.

```elixir
iex(joe@host)2> resume_info = Nostrum.Shard.Supervisor.disconnect(0)
%{
  session: "abcdefg",
  # ... 
}
iex(joe@host)3> :global.send(:target, resume_info)
```

On `mike@host`, receive it and start the shard:

```elixir
iex(mike@host)3> resume_info = receive do info -> info end
%{
  session: "abcdefg",
  # ...
}
iex(mike@host)4> Nostrum.Shard.Supervisor.reconnect(resume_info)
{:ok, #PID<0.421.0}
```

<!-- How the actual FUCK does this work? We are literally sending an anonymous
function over the wire. WTF? -->


> #### Handling bot setup in ready events {: .tip}
>
> It's common to handle bot setup such as registering commands in the `READY`
> event. However, when resuming the shard session on a different node, you will
> probably not receive this event. It is therefore recommended to move the
> handler for this event out of the consumer and into a function you can call as
> part of your takeover routine.

In your productive setup, you could have a dedicated globally registered process
handle "takeover" messages to start shards locally.

We have now transferred the shard session to the new node, ideally resuming with
0 loss. Note that a ratelimiter has been running on the new node ever since we
started our bot there, and nostrum has already started distributing requests
across both ratelimiters.

Let's now move our cache state to `mike@host` so we can stop `joe@host`. Note
that you might need to explicitly inform nostrum about your bot's name such that
it can figure out the table name, see `Nostrum.Bot` for details.

```elixir
iex(mike@host)5> alias Nostrum.Cache
iex(mike@host)6> :mnesia.move_table_copy(Cache.ChannelGuildMapping.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
iex(mike@host)7> :mnesia.move_table_copy(Cache.GuildCache.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
iex(mike@host)8> :mnesia.move_table_copy(Cache.MemberCache.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
iex(mike@host)9> :mnesia.move_table_copy(Cache.PresenceCache.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
iex(mike@host)10> :mnesia.move_table_copy(Cache.UserCache.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
iex(mike@host)11> alias Nostrum.Store
iex(mike@host)12> :mnesia.move_table_copy(Store.GuildShardMapping.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
iex(mike@host)13> :mnesia.move_table_copy(Store.UnavailableGuild.Mnesia.table(), :joe@host, node())
{:atomic, :ok}
```

<!-- TODO: Migrating multiple shards could probably be cleaner with some nostrum helpers. -->
<!-- TODO: Migrating caches could probably be cleaner with some nostrum helpers. -->
<!-- TODO: Migrating stores could probably be cleaner with some nostrum helpers. -->

Now, check `:mnesia.info/0`. All tables should be transferred to `mike@host`.

Your bot has been transferred. Note that until `joe@host` stops, it will still
run a ratelimiter and handle some REST requests, as the `Nostrum.Bot` supervisor
in there will still be running.

You can now stop the `Nostrum.Bot` supervisor for said bot on `joe@host`, or the
entire node, if you wish.


<!-- vim: set textwidth=80 sw=2 ts=2: -->
