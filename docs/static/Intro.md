# Intro
Nostrum is a an Elixir library that can be used to interact with Discord.

Nostrum currently supports the latest stable version of Elixir, v. 1.7.

With a platform like Discord, there are many moving parts and an attempt was made
to break these parts into smaller logical pieces.

To see documentation about a specific part of the library, please visit one of
the following -

 * [API](api.html) - Methods to interact with the RESTful API (and some other goodies).
 * [State](state.html) - `Caches` that hold the state of Discord that your bot can see.
 * [Events](events.html) - How you can handle real time events that your bot can see.
 * [Custom Consumers](consumers.html) - Information on defining custom consumer processes.

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

Add Nostrum as a dependency:

 *Stable*
```Elixir
def deps do
  [{:nostrum, "~> 0.3"}]
end
```

 *Dev*
```Elixir
def deps do
  [{:nostrum, git: "https://github.com/Kraigie/nostrum.git"}]
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

 - `dev` - This is added to enable Nostrum to be run completely stand alone for
 development purposes. `true` will cause Nostrum to spawn its own event consumers.
 If you have the dev flag set to true while running Nostrum alongside your
 application some of your events will be consumed. Defaults to `false`.
 - `log_full_events` - This will log the full payload received over the websocket.
 This is included primarily for debugging and testing purposes. Defaults to `false`.
 - `log_dispatch_events` - This will log dispatch events as they are received from the gateway.
 This is included primarily for debugging and testing purposes. Defaults to `false`. 
 - `custom_consumer` - For use when creating custom consumer processes. This disables
 all caching done internally, in lieu of sending dispatch events to the processes
 you specify. For more information see [creating custom consumers](consumers.html).
 Defaults to `false`.
 - `request_guild_members` - This will perform member chunking to retrieve a complete list of
 members for all guilds. This will increase start up time and memory usage by quite a bit.
 Defaults to `false`.
 - `fullsweep_after_default` - Sets the `fullsweep_after` flag for processes that can have
 irregularly high memory usage due to Discord payloads. This options will dramatically reduce the
 amount of memory used by some processes at the cost of increased CPU usage. This is useful if
 you're running your application under a memory constrained environment. This comes at the cost
 of increased CPU usage. By default, this option will only affect some processes. You can set
 this flag for *all* processes using environment variables or by [setting the system flag yourself](http://erlang.org/doc/man/erlang.html#system_flag-2).
 Defaults to whatever your system recommends, which is probably `65535`.

By default, Nostrum logs a lot of data using `Logger`. If you want to ignore
Logger debug data, include the following:
```Elixir
config :logger,
  level: :warn
```

Nostrum exposes the following metadata fields through logger:
 - `shard` - Id of the shard on which the event occured

To enable this metadata, logger can be configured as such:
```Elixir
config :logger, :console,
  metadata: [:shard]
```  

For more information on how this works, please see the Logger
[documentation](https://hexdocs.pm/logger/Logger.html#module-levels)
Nostrum takes advantage of `:debug`, `:warn`, and `:info`.

### Example Bot
A very simple example bot can be found
[here](https://github.com/Kraigie/nostrum/blob/a655b8bea1c96777ea6ec729493625b147899081/examples/event_consumer.ex).

A more complex bot can be found
[here](https://github.com/Kraigie/mark-hoff).
