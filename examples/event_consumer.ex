defmodule ExampleSupervisor do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    bot_options = %{
      consumer: MyBot.Consumer,
      wrapped_token: fn -> System.fetch_env!("BOT_TOKEN") end
    }

    children = [
      {Nostrum.Bot, {bot_options, []}}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

defmodule MyBot.Consumer do
  @behaviour Nostrum.Consumer

  alias Nostrum.Api.Message

  def handle_event({:MESSAGE_CREATE, msg, _ws_state}) do
    case msg.content do
      "!sleep" ->
        Message.create(msg.channel_id, "Going to sleep...")
        # This won't stop other events from being handled.
        Process.sleep(3000)

      "!ping" ->
        Message.create(msg.channel_id, "pyongyang!")

      "!raise" ->
        # This won't crash the entire Consumer.
        raise "No problems here!"

      _ ->
        :ignore
    end
  end

  # Ignore any other events
  def handle_event(_), do: :ok
end
