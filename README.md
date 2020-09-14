# Nostrum

[![Build Status](https://travis-ci.org/Kraigie/nostrum.svg?branch=master)](https://travis-ci.org/Kraigie/nostrum)
[![Join Discord](https://img.shields.io/badge/discord-join-7289DA.svg)](https://discord.gg/2Bgn8nW)

An [Elixir](http://elixir-lang.org/) library for the Discord API.

It is highly recommended to check out the
[documentation](https://kraigie.github.io/nostrum/) first. It includes all of the
information listed here and more.

## Installation
Add Nostrum as a dependency:

 *Stable*

 Stable documentation can be found [here](https://hexdocs.pm/nostrum/)
```elixir
def deps do
  [{:nostrum, "~> 0.4"}]
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
  token: "666", # The token of your bot as a string
  num_shards: 2 # The number of shards you want to run your bot under, or :auto.
```

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
  iex()> {:ok, pid_consumer} = ExampleConsumer.start_link
  {:ok, #PID<0.208.0>}
```

## Getting Help

If you need help, visit `#elixir_nostrum` on the unofficial Discord API guild!

[![Discord API](https://discord.com/api/guilds/81384788765712384/embed.png?style=banner3)](https://discord.gg/2Bgn8nW)

## License
[MIT](https://opensource.org/licenses/MIT)
