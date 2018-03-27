# Nostrum

An [Elixir](http://elixir-lang.org/) library for the Discord API.

It is highly recommended to check out the
[documentation](https://kraigie.github.io/nostrum/) first. It includes all of the
information listed here and more.

## Foreword
The version of this library hosted on Hex is severely outdated. Once the dependency
[gun](https://github.com/ninenines/gun) has a 2.0 release, a new package will be
released with the most up to date Elixir version (1.5.1 as of the time of this writing).

In the meantime it is recommended you use the version hosted here on GitHub.

*Thanks PixeL!*

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

For more information about the differences between dev and stable as well as
additional config parameters, please see the
[documentation](https://kraigie.github.io/nostrum/).

## Example Usage
The below module needs to be started in some fashion to capture events. See
[here](https://github.com/Kraigie/nostrum/blob/master/examples/event_consumer_supervisor.ex)
for a full example.

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
        Api.create_message(msg.channel_id, "I copy and pasted this code")
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

Although it's not recommended, you could start it from `iex`.
```Elixir
  iex()> ExampleConsumer.start
  {:ok, #PID<0.208.0>}
```

## Contributing
TODO

## License
[MIT](https://opensource.org/licenses/MIT)
