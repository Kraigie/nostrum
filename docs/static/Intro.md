# Intro
Nostrum is a an Elixir library that can be used to interact with Discord.

Nostrum currently supports the latest stable version of Elixir, v. 1.4.

With a platform like Discord, there are many moving parts and an attempt was made
to break these parts into smaller logical pieces.

To see documentation about a specific part of the library, please visit one of
the following -

 * [API](api.html) - Methods to interact with the RESTful API (and some other goodies).
 * [State](state.html) - `Caches` that hold the state of Discord that your bot can see.
 * [Events](events.html) - How you can handle real time events that your bot can see.

### Why Elixir?
From the Elixir website -
> Elixir is a dynamic, functional language designed for building scalable and
maintainable applications.

> Elixir leverages the Erlang VM, known for running low-latency, distributed and
fault-tolerant systems, while also being successfully used in web development
and the embedded software domain.

**How does Nostrum leverage these benefits?**

*Fault tolerance* - Nostrum attempts, where reasonable, to allow for different
parts of the application to fail with minimal harm. Supervisors are used heavily
to ensure that anything that breaks will be restarted. Nostrum attempts to avoid
most errors, but if they do occur (*and they will!*) it shouldn't be the end of
the world.

*Concurrency* - Concurrency is considered to be a first class citizen in Elixir.
Through the use of SMP and processes, information should travel through the
application quickly.

*Distributed* - Nostrum does not currently supported any sort of `Distributed`
mode out of the box. By default everything is ran on the one `node` that you
launch the application from. I'm open to suggestions as to how Nostrum could
leverage being spread across many nodes.

### Usage
There are two versions of Nostrum, a stable version released on Hex and a dev
version on GitHub. The dev version will be more up to date but will likely
have more errors.

When choosing whether to use dev or stable, there is one important distinction
to keep in mind. The WS lib that Nostrum uses does not support giving proper
close codes on WS disconnects. A fork is maintained (a fork of a fork of a fork)
that has been monkey patched to allow the proper sending of close codes.
Unfortunately, it doesn't seem like the original fork's author maintains the project
anymore. Seeing as you can't have a package on Hex with a git dependency, the stable
version will not have close codes until the current implementation is tested and
released on its own, or the maintainer of the fork decides to come back to life.

Add Nostrum as a dependency:

 *Dev*
```Elixir
def deps do
  [{:nostrum, git: "https://github.com/Kraigie/nostrum.git"}]
end
```

 *Stable*
```Elixir
def deps do
  [{:nostrum, "~> 0.1"}]
end
```

Edit or create your config file:

The file should be located at `/config/config.exs`. To run Nostrum you need the
following two fields:
```Elixir
config :nostrum,
  token: 666, # The token of your bot as a string
  num_shards: 2 # The number of shards you want to run your bot under, or :auto.
```
If you don't know what `num_shards` is or don't have your bot on a lot of guilds
you can omit the field and it will default to 1. You can also set this option to
`:auto` and Nostrum will automatically get the recommended number of shards.

The following fields are also supported:

 - `self_bot` - Runs the bot as a self bot. If you don't know what this is, then
 don't set it, or set it to `false`. Defaults to `false`.
 - `dev` - This is added to enable Nostrum to be run completely stand alone for
 development purposes. `true` will cause Nostrum to spawn its own event consumers.
 If you have the dev flag set to true while running Nostrum alongside your
 application some of your events will be consumed. Defaults to `false`.
 - `log_full_events` - This will log the full payload received over the websocket.
 This is included primarily for debugging and testing purposes. Defaults to `false`.

By default, Nostrum logs a lot of data using `Logger`. If you want to ignore
Logger debug data, include the following:
```Elixir
config :logger,
  level: :warn
```

For more information on how this works, please see the Logger
[documentation](https://hexdocs.pm/logger/Logger.html#module-levels)
Nostrum takes advantage of `:debug`, `:warn`, and `:info`.

### Example Bot
A very simple example bot can be found
[here](https://github.com/Kraigie/nostrum/blob/a655b8bea1c96777ea6ec729493625b147899081/examples/event_consumer.ex).

A more complex bot can be found
[here](https://github.com/Kraigie/mark-hoff).
