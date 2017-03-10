# Nostrum

An [Elixir](http://elixir-lang.org/) library for the Discord API.

It is highly recommended to check out the
[documentation](https://kraigie.github.io/nostrum/) first. It includes all of the
information listed here and more.

## Installation
Add Nostrum as a dependency:

 *Dev*
```Elixir
def deps do
  [{:nostrum, git: "https://github.com/Kraigie/nostrum.git"}]
end
```

 *Stable*

 Stable documentation can be found [here](https://hexdocs.pm/nostrum/)
```elixir
def deps do
  [{:nostrum, "~> 0.1"]
end
```

Ensure Nostrum is started before your application:
```elixir
def application do
  [applications: [:nostrum]]
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

For more information about the differences between dev and stable as well as
additional config parameters, please see the
[documentation](https://kraigie.github.io/nostrum/).

## Example Usage
```Elixir
defmodule ExampleConsumer do
  use Nostrum.Consumer
  alias Nostrum.Api

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  def handle_event({:MESSAGE_CREATE, {msg}, _ws_state}, state) do
    case msg.content do
      "ping!" ->
        Api.create_message(msg.channel.id, "I copy and pasted this code")
      _ ->
        :ignore
    end

    {:ok, state}
  end

  # Default event handler, if you don't include this, your consumer WILL crash if
  # you don't have a method definition for each event type.
  def handle_event(_, state) do
    {:ok, state}
  end
end
```

## Contributing
TODO

## License
[MIT](https://opensource.org/licenses/MIT)
