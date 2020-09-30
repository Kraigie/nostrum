defmodule EventConsumer.Consumer do
  use Nostrum.Consumer

  alias Nostrum.Api

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  def handle_event({:MESSAGE_CREATE, msg, _ws_state}) do 
    case msg.content do
      "!klk" ->
        message = "klk #{msg.author.username}, tranquilo bro ???"
        Api.create_message(msg.channel_id, message)

      "!ping" ->
        Api.create_message(msg.channel_id, "pong")

      "!raise" ->
        raise "No problems here!"

      _ ->
        :ignore
    end
  end

  def handle_event(_event) do
    :noop
  end
end