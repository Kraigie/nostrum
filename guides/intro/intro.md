# Introduction

nostrum is an Elixir library that can be used to interact with Discord.

To see documentation about a specific part of the library, please visit one of
the following:

* [API](api.html) - Methods to interact with the RESTful API (and some other goodies).
* [State](state.html) - Caches that keep information from Discord fresh at your disposal.
* [Events](events.html) - Handling events from Discord as they come in.
* [Voice](voice.html) - Playing audio through Discord voice channels.

## Setup

There are two versions of nostrum, a stable version released on Hex and a dev
version on GitHub. The dev version will be more up to date but will likely
have more errors.

```elixir
def deps do
  [{:nostrum, "~> 0.9"}]
  # Or, for bleeding edge changes:
  # [{:nostrum, github: "Kraigie/nostrum"}]
end
```

Next up, you need to configure nostrum. The most basic configuration is simply
setting a token to log in with:

```elixir
config :nostrum,
  token: "666"  # The token of your bot as a string
```


## Configuration options

Apart from the `token` field mentioned above, the following fields are also supported:

- `num_shards` - the amount of shards to run. Can be one of the following:
  - `:auto`: use the suggested amount of shards as provided by Discord.
  - *`num`*: a number of shards to run. nostrum will warn if this is not the
    recommended amount.
  - `{lowest, highest, total}`: start shards `lowest` to `highest`. `total`
    should contain the total amount of shards that your bot is expected to have.
    Useful for splitting a single bot across multiple servers, but see also [the
    multi-node documentation](../advanced/multi_node.md).
- `gateway_intents` - a list of atoms representing gateway intents for Nostrum
  to subscribe to from the Discord API. More information can be found in the
  [gateway intents](./gateway_intents.md) documentation page.
- `request_guild_members` - perform member chunking to retrieve a complete list
  of members for all guilds at startup. Depending on your [cache
  backend](../advanced/pluggable_caching.md), this may increase startup time
  and memory usage by quite a bit. Defaults to `false`.


### Voice-specific

- `ffmpeg` - Specifies the path to the `ffmpeg` executable for playing audio.
  Defaults to `"ffmpeg"`.
- `youtubedl` - Specifies the path to the `youtube-dl` executable for playing
  audio with youtube-dl support. Defaults to `"youtube-dl"`.
- `streamlink` - Specifies the path to the `streamlink` executable for playing
  livestream audio with streamlink support. Defaults to `"streamlink"`.
- `audio_timeout` - Milliseconds that input must begin generating audio by
  upon invoking `play`. More information about this option can be found in the
  [voice](./voice.html) documentation page. Defaults to `20_000` (20s).
- `audio_frames_per_burst` - Number of opus frames to send at a time while
  playing audio. More information about this option can be found in the
  [voice](./voice.html) documentation page. Defaults to `10`.
- `voice_auto_connect` - This will determine if Nostrum automatically connects
  to voice websockets gateways upon joining voice channels. If set to `false`
  but you still wish to connect to the voice gateway, you can do so manually
  by calling `Nostrum.Voice.connect_to_gateway/1` after joining a voice
  channel. Defaults to `true`.


### Development & debugging

- `log_full_events` - This will log the full payload received over the
  websocket. Defaults to `false`.
- `log_dispatch_events` - This will log dispatch events as they are received
  from the gateway. Defaults to `false`.
- `fullsweep_after_default` - Sets the `fullsweep_after` flag for processes
  that can have irregularly high memory usage due to Discord payloads. This
  options will dramatically reduce the amount of memory used by some processes
  at the cost of increased CPU usage. This is useful if you're running your
  application under a memory constrained environment. This comes at the cost
  of increased CPU usage. By default, this option will only affect some
  processes. You can set this flag for *all* processes using environment
  variables or by [setting the system flag
  yourself](http://erlang.org/doc/man/erlang.html#system_flag-2). Defaults to
  whatever your system recommends, which is probably `65535`.


### Internal options

The following options are only used for testing nostrum itself.

- `dev` - This is added to enable Nostrum to be run completely stand alone for
  development purposes. `true` will cause Nostrum to spawn its own event
  consumers. If you have the dev flag set to true while running Nostrum
  alongside your application some of your events will be consumed. Defaults to
  `false`.


## Logging

nostrum uses Elixir's standard logger to inform you about regular and irregular
events. Normal messages include Discord-requested shard reconnections and the
`IDENTIFY` and `READY` events.

The following metadata fields through logger:

 - `shard` - Id of the shard on which the event occurred
 - `guild` - Name of the guild on which the voice connection event occurred
 - `channel` - Name of the channel on which the voice connection event occurred

To enable this metadata, logger can be configured as such:
```elixir
config :logger, :console,
  metadata: [:shard, :guild, :channel]
```  

For more information on how this works, and how to change the logging
configuration for nostrum on its own, please see the [Logger
documentation](https://hexdocs.pm/logger/Logger.html)

### Why Elixir?

From the Elixir website -

> Elixir is a dynamic, functional language designed for building scalable and
> maintainable applications.

> Elixir leverages the Erlang VM, known for running low-latency, distributed
> and fault-tolerant systems, while also being successfully used in web
> development and the embedded software domain.

**How does Nostrum leverage these benefits?**

- **Fault tolerance**: nostrum attempts, where reasonable, to allow for
  different parts of the application to fail with minimal harm. Supervisors are
  used heavily to ensure that anything that breaks will be restarted. Nostrum
  attempts to avoid most errors, but if they do occur (*and they will!*) it
  shouldn't be the end of the world.

- **Concurrency** - Concurrency is considered to be a first class citizen in
  Elixir. Through the use of SMP and processes, information should travel
  through the application quickly.

- **Distributed**: nostrum does not currently support full distribution of all
  components out of the box - that would fall out of the scope of the library.
  However, see the [multi-node](../advanced/multi_node.md) document for
  provided functionality. 


## Example bots

- A simple bot that consumes events from the gateway can be found [at
  `examples/event_consumer.ex`](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer.ex).

- An example bot that plays audio through voice channels can be found [at
  `examples/audio_player_example.ex`](https://github.com/Kraigie/nostrum/blob/master/examples/audio_player_example.ex).

- A properly structured bot with commands utilizing
  [nosedrum](https://github.com/jchristgit/nosedrum) can be found [in this
  GitHub repository](https://github.com/kshannoninnes/sample_bot)

- A rather large and complex bot, bolt, can be found
  [here](https://github.com/jchristgit/bolt).



<!-- vim: set textwidth=80 sw=2 ts=2: -->
