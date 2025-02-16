# Introduction

nostrum is an Elixir library that can be used to interact with Discord.

To see documentation about a specific part of the library, please visit one of
the following:

* [API](api-1.html) - Methods to interact with the RESTful API (and some other goodies).
* [State](state.html) - Caches that keep information from Discord fresh at your disposal.
* [Events](event_handling.html) - Handling events from Discord as they come in.
* [Voice](voice-2.html) - Playing audio through Discord voice channels.

This introductory document assumes some basic familarity with Elixir, although
we aim to make it easy to understand for newcomers as well.

## Project setup

If you are starting out on a completely new project, please [install
Elixir](https://elixir-lang.org/install.html) first. Once you have Elixir
installed, run the following to set up your bot repository:

```elixir
mix new my_bot --sup
```

Give it any name you like. `--sup` is needed to generate a supervisor, more on
this in a moment.

Head to the project directory. `mix new` has generated a basic structure for
your bot for you.

## Adding the dependency

We need to add nostrum as a dependency to be able to use it. Dependencies are
tracked in `mix.exs`. Open this file with your favourite text editor, and head
down to the `def deps do` function. Update it as follows:

```elixir
def deps do
  [{:nostrum, "~> 0.10"}]
  # Or, for bleeding edge changes:
  # [{:nostrum, github: "Kraigie/nostrum"}]
end
```

Afterwards, run `mix deps.get` to fetch dependencies.

> #### Stable and development versions
>
> There are two versions of nostrum, a stable version released on Hex and a dev
> version on GitHub. The development version will be more up to date but might
> suffer from some bugs - we generally patch them quickly if you report them on
> the issue tracker. It's appreciated if you can use the development version to
> help test nostrum.

## Consumer setup

Next up, you need to define a consumer - a module which handles events, see the
`Nostrum.Consumer` docs. In `lib/my_bot/example_consumer.ex`, define the
following:

A basic consumer could look like the following:

```elixir
defmodule MyBot.Consumer do
  @behaviour Nostrum.Consumer

  alias Nostrum.Api.Message

  def handle_event({:MESSAGE_CREATE, msg, _ws_state}) do
    case msg.content do
      "!hello" ->
        {:ok, _message} = Message.create(msg.channel_id, "Hello, world!")

      _ ->
        :ignore
    end
  end

  # Ignore any other events
  def handle_event(_), do: :ok
end
```

Finally, you need to add `Nostrum.Bot` to your app's supervisor tree in
`lib/my_bot/application.ex` to start the bot as part of your app's startup:

```elixir
defmodule MyBot.Application do
  use Application

  @impl true
  def start(_init_arg) do
    bot_options = %{
      consumer: MyBot.Consumer,
      intents: [:direct_messages, :guild_messages, :message_content],
      wrapped_token: fn -> System.fetch_env!("BOT_TOKEN") end,
    }
    children = [
      {Nostrum.Bot, {bot_options, []}}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

Note that this assumes you set the environment variable `$BOT_TOKEN` to your bot
token, for example via `export BOT_TOKEN=my-bot-token`. This is recommended, but
feel free to use any other configuration method you like.

> #### Message intents {: .info}
>
> Due to Discord API changes, _in order to receive message content_ (e.g.  for
> non-slash commands or moderation tools), you need to have the "Message Content
> Intent" enabled on your [Bot's application
> settings](https://discord.com/developers/applications/), and the
> `:message_content` intent specified in the `[:nostrum, :gateway_intents]`
> configuration key.


## Configuration options

nostrum supports the following global configuration options:

- `request_guild_members` - perform member chunking to retrieve a complete list
  of members for all guilds at startup. Depending on your [cache
  backend](../advanced/pluggable_caching.md), this may increase startup time
  and memory usage by quite a bit. Defaults to `false`.
- `gateway_compression` - use either `:zlib` (default) or `:zstd` for compression
  of messages from the Discord gateway. See the documentation on
  [Gateway Compression](../advanced/gateway_compression.md) for more information.


### Voice-specific

- `ffmpeg` - Specifies the path to the `ffmpeg` executable for playing audio.
  Defaults to `"ffmpeg"`.
- `youtubedl` - Specifies the path to the `youtube-dl` executable for playing
  audio with youtube-dl support. Defaults to `"youtube-dl"`.
- `streamlink` - Specifies the path to the `streamlink` executable for playing
  livestream audio with streamlink support. Defaults to `"streamlink"`.
- `audio_timeout` - Milliseconds that input must begin generating audio by
  upon invoking `play`. More information about this option can be found in the
  [voice](./voice-2.html) documentation page. Defaults to `20_000` (20s).
- `audio_frames_per_burst` - Number of opus frames to send at a time while
  playing audio. More information about this option can be found in the
  [voice](./voice-2.html) documentation page. Defaults to `10`.
- `voice_auto_connect` - This will determine if Nostrum automatically connects
  to voice websockets gateways upon joining voice channels. If set to `false`
  but you still wish to connect to the voice gateway, you can do so manually
  by calling `Nostrum.Voice.connect_to_gateway/1` after joining a voice
  channel. Defaults to `true`.
- `voice_encryption_mode` - Defaults to `:aes256_gcm`. More information about this
  option can be found [here](./voice-2.html#encryption-modes).


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
- `force_http1` - Set to `true` if you wish to disable automatic use of HTTP 2
  or newer HTTP versions for API requests to Discord. Useful to diagnose issues
  with ratelimiter connections during abnormal network conditions.


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

- A simple bot that consumes events from the gateway can be found in the docs of
  the `Nostrum.Consumer` module.

- An example bot that plays audio through voice channels can be found in the
  docs of the `Nostrum.Voice` module.

- A properly structured bot with commands utilizing
  [nosedrum](https://github.com/jchristgit/nosedrum) can be found [in this
  GitHub repository](https://github.com/kshannoninnes/sample_bot)

- A rather large and complex bot, bolt, can be found
  [here](https://github.com/jchristgit/bolt).



<!-- vim: set textwidth=80 sw=2 ts=2: -->
