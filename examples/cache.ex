# To get this example going, run `iex -S mix` and `ExampleSupervisor.start_link`.
# This will start the event consumer under a supervisor.
defmodule ExampleSupervisor do
  use Supervisor

  def start_link(args \\ []) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [ExampleConsumer]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

# The event consumer that will be handling all incoming events.
defmodule ExampleConsumer do
  use Nostrum.Consumer

  def start_link do
    Consumer.start_link(__MODULE__)
  end

  # We only need to write specific event handlers for the events we are interested in,
  # which in this case, will be the handling of new Discord messages,
  # for other cases, there is a catch all handler at the end of the module.
  def handle_event({:MESSAGE_CREATE, message, _ws_state}) do
    ExampleCommands.command(message)
  end

  # The catch all event case that takes and ignores all events you are not
  # interested in, if you do not have this, or have not defined literally
  # every event case yourself (what an absolute madlad if you have),
  # the consumer will crash, not having any function signature to match upon.
  # By the way, the return atom stands for "no-op", shorthand for "no operation",
  # however, if you read it as "noop" and quietly chuckle to yourself every time you see it,
  # since it just sounds like a silly way of saying "nope", know, that you are not alone.
  def handle_event(_event), do: :noop
end

# Our basic, example command handler that will be taking the message
# and actually doing something with it, wow, amazing!
defmodule ExampleCommands do
  import Nostrum.Snowflake, only: [is_snowflake: 1]

  alias Nostrum.Api
  alias Nostrum.Cache.{ChannelCache, GuildCache, UserCache}
  alias Nostrum.Struct.User

  # Fetch the defined prefix from our config, however,
  # if you have not defined it in your config yet, fear not,
  # the line below will default it to "!"
  @prefix Application.get_env(:example_app, :prefix, "!")

  defp get_cached_with_fallback(id, cache, api) do
    case cache.(id) do
      {:ok, _} = data -> data
      {:error, _} -> api.(id)
    end
  end

  # The command that will be handling our message.
  # Pattern matching's pretty neat by the way, a nice way to define a shape for the data we accept
  # and have it reject the rest, also gets rid of a good chunk of all that pesky condition checking
  # that you would normally see, trying for that data shape that we need, destructuring's also pretty neat.
  # For this, a simple `userinfo` command that accepts a user id and returns a message
  # containing some things you would need to fetch from the cache (or the API, as a backup).
  def command(%{
        content: @prefix <> "userinfo " <> message_user_id,
        channel_id: channel_id,
        guild_id: guild_id
      })
      when guild_id != nil do
    # Our happy path for sending a complete userinfo response.
    # Pretty standard stuff, maybe apart from additionally wrapping both sides in a tuple
    # with an extra identifier, that's just to make failure cases more obvious.
    with {{user_id, _binary}, :parse_id} when is_snowflake(user_id) <-
           {Integer.parse(message_user_id), :parse_id},
         {{:ok, user}, :user} <-
           {get_cached_with_fallback(user_id, &UserCache.get/1, &Api.get_user/1), :user},
         {{:ok, %{name: channel_name}}, :channel} <-
           {get_cached_with_fallback(channel_id, &ChannelCache.get/1, &Api.get_channel/1),
            :channel},
         {{:ok, %{name: guild_name}}, :guild} <-
           {get_cached_with_fallback(guild_id, &GuildCache.get/1, &Api.get_guild/1), :guild} do
      Api.create_message(
        channel_id,
        """
        ID #{message_user_id} belongs to: #{User.full_name(user)}
        Message sent in channel: ##{channel_name}
        Channel belongs to guild: #{guild_name}
        """
      )
    else
      # Failure case for parsing and (somewhat) validating the User ID.
      # Pretty easy to tell which point of failure this case handles,
      # if we would match only on the return value of `Integer.parse/2`,
      # the error value for that function is literally just `:error`,
      # not very descriptive, is it... would not be easy to tell from a glance
      # where in the happy path a failure occurred, if you do not know the error
      # return values of the happy path functions in that very moment you were reading this.
      # This kind of pattern gets pretty handy when return values are quite descriptionless or
      # you have multiple functions that return the same thing, but also need to be handled differently.
      # Either way, a pattern I do not see enough of, but would like to, where it's needed.
      {_invalid_id, :parse_id} ->
        Api.create_message(channel_id, "Make sure you entered a valid User ID")

      # The cache + API failure case with the additional resource identifier, pretty straight forward,
      # resource type for clarity, and then whatever the API throws at us as the reason, though the
      # stringified API reason is for development purposes, not something you would throw at a regular
      # user, but you know, this is just an example after all, make it all nice and pretty.
      {{:error, reason}, resource} ->
        Api.create_message(channel_id, "Unable to retrieve #{resource}: #{inspect(reason)}")
    end
  end

  # Just like handling events, a fall through `command` case to catch and ignore unwanted cases,
  # otherwise it will go boom, KABLAM, "OH GOD NO!", *insert mushroom cloud here*, "AAAAH, MY FACE,
  # IT'S MELTING!", well not really, since we have supervision trees, fault tolerance and the absolute,
  # massive powerhouse of awesomeness that the BEAM is and all that jazz, but you know, you catch my drift.
  # Also, "noop", *giggles*.
  def command(_invalid_command), do: :noop
end
