defmodule ExampleSupervisor do
  def start do
    import Supervisor.Spec

    # List comprehension creates a consumer per cpu core
    children =
      for i <- 1..System.schedulers_online(),
          do: Supervisor.child_spec({ExampleConsumer, []}, [:consumer, i])

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

defmodule ExampleConsumer do
  use Nostrum.Consumer

  alias Nostrum.Api

  require Logger

  def start_link do
    Consumer.start_link(__MODULE__, :state)
  end

  def handle_event({:MESSAGE_CREATE, {msg}, ws_state}, state) do
    # Let's do a little command 'parsing'
    # We first grab the content, split it into an array and then convert the array to a tuple
    args = msg.content |> String.split(" ") |> List.to_tuple()

    # Check if first arg is the command
    if elem(args, 0) === "!userinfo" do
      # First, we check if there are 2 arguments | This being `!userinfo ID` | Otherwise we error out in the `false` clause.
      # Then we grab the second arg | `ID`
      # Parse it into an integer, otherwise error out within the `:error` clause.
      # Then, we try and grab the data from the cache.
      # If this fails, it will try the next thing in the clause which in this case would be sending out a request to Discord.
      # If this fails, it will go over into the else statement down below and print out that it can't find anyone/anything.
      with true <- tuple_size(args) == 2,
           second_arg = elem(args, 1),
           {user_id, _binary} <- Integer.parse(second_arg),
           {:ok, user} <- Nostrum.Cache.UserCache.get(id: user_id),
           Api.get_user(user_id),
           {:ok, channel} <- Nostrum.Cache.ChannelCache.get(id: msg.channel_id),
           Api.get_channel(msg.channel_id),
           {:ok, guild} <- Nostrum.Cache.GuildCache.get(channel.guild_id),
           Api.get_guild(channel.guild_id) do
        Api.create_message!(
          msg.channel_id,
          "#{user_id} belongs to a person named: #{user.username}\nMessage sent in: #{
            channel.name
          }\nChannel is in: #{guild.name}"
        )
      else
        # For this, we just print out we can't find anyone while using the error from the with statement. If it can't find someone, it will print out `user_not_found` as the reason.
        {:error, reason} ->
          Api.create_message!(msg.channel_id, "Sorry, something went wrong: #{reason}")

        # They typed an invalid input, probably due to using letters rather than numbers.
        :error ->
          Api.create_message!(msg.channel_id, "Make sure the User ID is only numbers")

        # There wasn't 2 elements in there, so it returned false.
        false ->
          Api.create_message!(msg.channel_id, "Please supply a User ID")
      end
    end

    {:ok, state}
  end

  # Default event handler, if you don't include this, your consumer WILL crash if
  # you don't have a method definition for each event type.
  def handle_event(_, state) do
    {:ok, state}
  end
end
