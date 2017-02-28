# Intro
Mixcord is a an Elixir library that can be used to interact with Discord.

Mixcord currently supports the latest stable version of Elixir, v. 1.4.

With a platform like Discord, there are many moving parts and an attempt was made
to break these parts into smaller logical pieces.

To see documentation about a specific part of the library, please visit one of
the following -

 * [API](API.md) - Methods to interact with the RESTful API (and some other goodies).
 * [State](State.md) - `Caches` that hold the state of Discord that your bot can see.
 * [Events](Events.md) - How you can handle real time events that your bot can see.

### Why Elixir?
From the Elixir website -
> Elixir is a dynamic, functional language designed for building scalable and
maintainable applications.

>Elixir leverages the Erlang VM, known for running low-latency, distributed and
fault-tolerant systems, while also being successfully used in web development
and the embedded software domain.

**How does Mixcord leverage these benefits?**

*Fault tolerance* - Mixcord attempts, where reasonable, to allow for different
parts of the application to fail with minimal harm. Supervisors are used heavily
to ensure that anything that breaks will be restarted. Mixcord attempts to avoid
most errors, but if they do occur (*and they will!*) it shouldn't be the end of
the world.

*Concurrency* - Concurrency is considered to be a first class citizen in Elixir.
Through the use of SMP and processes, information should travel through the
application quickly.

*Distributed* - Mixcord does not currently supported any sort of `Distributed`
mode out of the box. By default everything is ran on the one `node` that you
launch the application from. I'm open to suggestions as to how Mixcord could
leverage being spread across many nodes.

### Usage
There are two versions of Mixcord, a stable version released on Hex and a dev
version on GitHub. The dev version will be more up to date but will likely
have more errors.

When choosing whether to use dev or stable, there is one important distinction
to keep in mind. The WS lib that Mixcord uses does not support giving proper
close codes on WS disconnects. A fork is maintained (a fork of a fork of a fork)
that has been monkey patched to allow the proper sending of close codes.
Unfortunately, it doesn't seem like the original fork's author maintains the project
anymore. Seeing as you can't have a package on Hex with a git dependency, the stable
version will not have close codes until the current implementation is tested and
released on its own, or the maintainer of the fork decides to come back to life.

Add Mixcord as a dependency:

 *Dev*
```Elixir
def deps do
  [{:mixcord, git: "https://github.com/Kraigie/mixcord.git"}]
end
```

 *Stable*
```Elixir
def deps do
  [{:mixcord, "~> 1.0"]
end
```

Ensure Mixcord is started before your application:
```
def application do
  [applications: [:mixcord]]
end
```

Edit or create your config file:

The file should be located at `/config/config.exs`. To run Mixcord you need the
following two fields:
```Elixir
config :mixcord,
  token: 666, # The token of your bot as a string
  num_shards: 2 # The number of shards you want to run your bot under.
```
If you don't know what `num_shards` is or don't have your bot on a lot of guilds
you can omit the field and it will default to 1.

You can include a third field `dev` which is a `boolean`. This is added to enable
Mixcord to be run completely stand alone for development purposes. By default,
`true` will cause Mixcord to spawn its own event consumers. If you have the dev
flag set to true while running Mixcord alongside your application some of your
events will be consumed.

By default, Mixcord logs a lot of data using `Logger`. If you want to ignore
Logger debug data, include the following:
```Elixir
config :logger,
  level: :warn
```

For more information on how this works, please see the Logger
[documentation](https://hexdocs.pm/logger/Logger.html#module-levels)
Mixcord takes advantage of `:debug`, `:warn`, and `:info`.

### Example Bot
A very simple example bot can be found
[here](https://github.com/Kraigie/mixcord/blob/master/examples/event_consumer.ex).

A more complex bot can be found
[here](https://github.com/Kraigie/mark-hoff).
