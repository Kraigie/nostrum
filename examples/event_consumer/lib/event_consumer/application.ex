defmodule EventConsumer.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      EventConsumer.Consumer,  # Simple Consumer
      EventConsumer.ConsumerWithCache  # Consumer with cache configurated
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: EventConsumer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
