# Intro
Nostrum is a an Elixir library that can be used to interact with Discord.

Nostrum currently supports versions of Elixir at or above v. 1.11.

With a platform like Discord, there are many moving parts and an attempt was made
to break these parts into smaller logical pieces.

To see documentation about a specific part of the library, please visit one of
the following -

 * [API](api.html) - Methods to interact with the RESTful API (and some other goodies).
 * [State](state.html) - `Caches` that hold the state of Discord that your bot can see.
 * [Events](events.html) - How you can handle real time events that your bot can see.
 * [Voice](voice.html) - Playing audio through Discord voice channels.

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
```elixir
def deps do
  [{:nostrum, "~> 0.7"}]
end
```

 *Dev*
```elixir
def deps do
  [{:nostrum, git: "https://github.com/Kraigie/nostrum.git"}]
end
```

Edit or create your config file:

The file should be located at `/config/config.exs`. To run Nostrum you need the
following two fields:
```elixir
config :nostrum,
  token: "666"  # The token of your bot as a string
```

The following fields are also supported:

 - `num_shards` - A fixed number of shards to run, or `:auto` to have Nostrum determine it automatically. Defaults to `:auto`.
 - `ffmpeg` - Specifies the path to the `ffmpeg` executable for playing audio. Defaults to `"ffmpeg"`.
 - `youtubedl` - Specifies the path to the `youtube-dl` executable for playing audio with youtube-dl support. Defaults to `"youtube-dl"`.
 - `streamlink` - Specifies the path to the `streamlink` executable for playing livestream audio with streamlink support. Defaults to `"streamlink"`.
 - `gateway_intents` - This field takes a list of atoms representing gateway intents for Nostrum to subscribe to from the Discord API. More information can be found in the [gateway intents](gateway-intents.html) documentation page.
 - `audio_timeout` - Milliseconds that input must begin generating audio by upon invoking `play`. More information about this option can be found in the [voice](voice.html) documentation page. Defaults to `20_000` (20s).
 - `audio_frames_per_burst` - Number of opus frames to send at a time while playing audio. More information about this option can be found in the [voice](voice.html) documentation page. Defaults to `10`.
 - `voice_auto_connect` - This will determine if Nostrum automatically connects to voice websockets gateways upon joining voice channels. If set to `false` but you still wish to connect to the voice gateway, you can do so manually by calling `Nostrum.Voice.connect_to_gateway/1` after joining a voice channel. Defaults to `true`.
 - `dev` - This is added to enable Nostrum to be run completely stand alone for
 development purposes. `true` will cause Nostrum to spawn its own event consumers.
 If you have the dev flag set to true while running Nostrum alongside your
 application some of your events will be consumed. Defaults to `false`.
 - `log_full_events` - This will log the full payload received over the websocket.
 This is included primarily for debugging and testing purposes. Defaults to `false`.
 - `log_dispatch_events` - This will log dispatch events as they are received from the gateway.
 This is included primarily for debugging and testing purposes. Defaults to `false`. 
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
```elixir
config :logger,
  level: :warn
```

Nostrum exposes the following metadata fields through logger:
 - `shard` - Id of the shard on which the event occurred
 - `guild` - Name of the guild on which the voice connection event occurred
 - `channel` - Name of the channel on which the voice connection event occurred

To enable this metadata, logger can be configured as such:
```elixir
config :logger, :console,
  metadata: [:shard, :guild, :channel]
```  

For more information on how this works, please see the Logger
[documentation](https://hexdocs.pm/logger/Logger.html#module-levels)
Nostrum takes advantage of `:debug`, `:warn`, and `:info`.

### Example Bot
A very simple example bot can be found
[here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer.ex).

A more complex bot can be found [here](https://github.com/jchristgit/bolt).

An example bot that plays audio through voice channels can be found [here](https://github.com/Kraigie/nostrum/blob/master/examples/audio_player_example.ex).
