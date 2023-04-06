# Nostrum

[![Build Status](https://github.com/Kraigie/nostrum/workflows/Test%20&%20Lint/badge.svg)](https://github.com/Kraigie/nostrum/actions)
[![Join Discord](https://img.shields.io/badge/discord-join-7289DA.svg)](https://discord.gg/2Bgn8nW)

An [Elixir](http://elixir-lang.org/) library for the Discord API.

It is highly recommended to check out the
[documentation](https://kraigie.github.io/nostrum/) first. It includes all of the
information listed here and more.

## Installation

It is recommended to use a **stable** release by specifying a published
version from Hex:

```elixir
def deps do
  [{:nostrum, "~> 0.6"}]
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

```Elixir
defmodule ExampleConsumer do
  use Nostrum.Consumer

  alias Nostrum.Api

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  def handle_event({:MESSAGE_CREATE, msg, _ws_state}) do
    case msg.content do
      "ping!" ->
        Api.create_message(msg.channel_id, "I copy and pasted this code")
      _ ->
        :ignore
    end
  end

  # Default event handler, if you don't include this, your consumer WILL crash if
  # you don't have a method definition for each event type.
  def handle_event(_event) do
    :noop
  end
end
```

Although it's recommended to run under a supervisor, you could start it from `iex`.
```Elixir
  iex()> ExampleConsumer.start
  {:ok, #PID<0.208.0>}
```

## Getting Help

If you need help, visit `#elixir_nostrum` on the unofficial Discord API guild!

[![Discord API](https://discord.com/api/guilds/81384788765712384/embed.png?style=banner3)](https://discord.gg/2Bgn8nW)

## Contributors

<a align="center" href="https://github.com/Kraigie/nostrum/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=Kraigie/nostrum" />
</a>

## License
[MIT](https://opensource.org/licenses/MIT)
