# Multi-node

At the moment, you can run nostrum in highly available mode across multiple
nodes via OTP's distributed application support, see below. Support for properly
distributing nostrum across multiple nodes and using them as one big entity is
not supported (yet).

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


### Bundling nostrum with our app

We want to colocate nostrum with our app to allow it to move around as our
application is moved around. For this, utilize OTP's [included
applications](https://www.erlang.org/doc/design_principles/included_applications.html)
feature to include nostrum into our supervision tree. You also need to
explicitly include nostrum's dependencies to ensure they are started, as the
regular nostrum application startup won't handle it for you. This can be done by
changing your application definition in `mix.exs` as follows:

```elixir
  def application do
    [
      mod: {MyBot.Application, []},
      included_applications: [:nostrum],
      # You can see this with `mix app.tree nostrum`
      extra_applications: [:certifi, :gun, :inets, :jason, :kcl, :mime]
      # ...
    ]
  end
```

You also need to set `runtime: false` for `:nostrum` itself in your
dependencies, and any dependencies of your app that depend on `:nostrum`, such
as command frameworks like `:nosedrum`:

```elixir
  defp deps do
    [
      {:nostrum, "~> 0.9", runtime: false},
      # {:nosedrum, "~> 0.6", runtime: false},
    ]
  end
```

You now need to add nostrum to your applications' children to start it as part
of your app:

```elixir
  def start(type, args) do
    children = [
      Nostrum.Application,
      # ...
    ]
  end
```

If you want to run some logic ahead of starting nostrum, you can naturally also
put it later into the list.

You can start your bot now, and it's going to run. If you look at your
bot's application in `:observer`, you will see that nostrum has now become one
with your bot. We call that integration engineering.

Now that our app bundles everything it needs with itself, this means starting
our app will also starting nostrum, and stopping will also stop nostrum. We need
this for step two.


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

At present, nostrum can not perform any state synchronization between nodes, it
is an effective restart from scratch. For most bots, this type of failover will
be sufficient.


<!-- vim: set textwidth=80 sw=2 ts=2: -->
