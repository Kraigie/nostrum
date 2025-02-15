_ = """
  This module exists primarily for documentation/testing purposes
"""

defmodule DummySupervisor do
  @moduledoc false

  use Supervisor

  def start_link(_args) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init(_args) do
    bot_options = %{
      consumer: DummyConsumer,
      intents: [:direct_messages, :guild_messages, :message_content],
      wrapped_token: fn -> System.fetch_env!("BOT_TOKEN") end,
    }
    children = [
      {Nostrum.Bot, {bot_options, []}}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule DummyConsumer do
  @moduledoc false
  @behaviour Nostrum.Consumer

  require Logger

  def handle_event({:MESSAGE_CREATE, message, _}) do
    Logger.debug(fn -> "Message received: #{inspect(message.content)}" end)
  end

  def handle_event({event_name, _, _}) do
    Logger.debug(fn -> "User would handle #{event_name} here" end)
  end

  def handle_event(_), do: :ok
end
