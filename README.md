# Nostrum

[![Build Status](https://github.com/Kraigie/nostrum/workflows/Test%20&%20Lint/badge.svg)](https://github.com/Kraigie/nostrum/actions)
[![Join Discord](https://img.shields.io/badge/discord-join-7289DA.svg)](https://discord.gg/2Bgn8nW)
[![hex.pm](https://img.shields.io/hexpm/v/nostrum.svg)](https://hex.pm/packages/nostrum/)


An [Elixir](http://elixir-lang.org/) library for the Discord API. nostrum
supports the following:

- Clean REST API implementation and ratelimiting.
- Documented structs and abstractions over API objects.
- Support for multi-node distribution of caches and internal state.
- Listening and sending voice data, with helpers for common functionality.
- Automatic, configurable maintenance of local caches of Discord data, with
  extensive query support and cache swapping functionality.

It is highly recommended to check out the [release
documentation](https://hexdocs.pm/nostrum/) first. It includes all of the
information listed here and more. **This README is for the master branch**,
which includes recent developments and may be unstable. If you want to live on
the edge regardless, you can check the [pre-release
documentation](https://kraigie.github.io/nostrum/).

## Installation

It is recommended to use a **stable** release by specifying a published
version from Hex:

```elixir
def deps do
  [{:nostrum, "~> 0.10"}]
end
```

For stable installations, documentation can be found at
https://hexdocs.pm/nostrum. However, if you want the latest changes and help
test the library, you can also install directly from GitHub:

```elixir
def deps do
  [{:nostrum, github: "Kraigie/nostrum"}]
end
```

Documentation for master can be found at https://kraigie.github.io/nostrum/.

Edit or create your config file at `/config/config.exs`. To run Nostrum you
need the following two fields:

```elixir
config :nostrum,
  token: "666" # The token of your bot as a string
```

> **Note:** Due to Discord API changes, _in order to receive message content_ (e.g.
for non-slash commands or moderation tools), you need to have the "Message
Content Intent" enabled on your [Bot's application
settings](https://discord.com/developers/applications/), and the
`:message_content` intent specified in the `[:nostrum, :gateway_intents]`
configuration key.

For more information about the differences between dev and stable as well as
additional config parameters, please see the
[documentation](https://kraigie.github.io/nostrum/).

## Example Usage
The below module needs to be started in some fashion to capture events. See
[here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer.ex)
for a full example.

```elixir
defmodule ExampleConsumer do
  use Nostrum.Consumer

  alias Nostrum.Api.Message

  def handle_event({:MESSAGE_CREATE, msg, _ws_state}) do
    case msg.content do
      "ping!" ->
        Message.create(msg.channel_id, "I copy and pasted this code")
      _ ->
        :ignore
    end
  end
end
```

You should start this under a supervisor or application:

```elixir
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [ExampleConsumer]
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

For testing, you can start it from `iex`:

```elixir
iex()> ExampleConsumer.start_link()
{:ok, #PID<0.208.0>}
```

## Getting Help

If you need help, visit `#elixir_nostrum` on the unofficial Discord API guild!

[![Discord API](https://discord.com/api/guilds/81384788765712384/embed.png?style=banner3)](https://discord.gg/2Bgn8nW)

## Testimonials

> my first choice is always nostrum
> 
> **- big nutty, javascript developer**

> i would feed my baby the latest nostrum release
>
> **- also big nutty**

> i've used nostrum for 6 years and i have never been let down. [...]
>
> **- Broman, Discord API Expert**

## License
[MIT](https://opensource.org/licenses/MIT)
