# To get this example going, run `iex -S mix` and `ExampleSupervisor.start_link`.
# This will start the event consumer under a supervisor.
defmodule CacheExampleSupervisor do
  use Supervisor

  def start_link(args \\ []) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [CacheExampleConsumer]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

# The event consumer that will be handling all incoming events.
defmodule CacheExampleConsumer do
  use Nostrum.Consumer

  # We only need to write event handlers for the events we are interested in,
  # the rest will go to the catch-all case to be ignored.
  def handle_event({:MESSAGE_CREATE, message, _ws_state}) do
    ExampleCommands.command(message)
  end

  # The catch-all event case that takes the rest of the events.
  # If you do not have this, or have not defined literally
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

  # The command that will be handling our message. Use pattern matching to
  # enforce the data shape that you accept, while also destructuring data for convenience.
  # This also helps avoiding unnecessary conditional checks in the function body,
  # that would usually introduce a need for early returns in other languages.
  # For this, a simple `userinfo` command that accepts a user id and returns a message,
  # containing information you would be fetching from the cache (or the API, as a backup).
  def command(%{
        content: @prefix <> "userinfo " <> message_user_id,
        channel_id: channel_id,
        guild_id: guild_id
      })
      when guild_id != nil do
    # Our happy path for sending a complete userinfo response.
    # We'll wrap both sides of the first one with an extra identifier,
    # so we can more easily match on cases where parsing would fail.
    with {{user_id, _binary}, :parse_id} when is_snowflake(user_id) <-
           {Integer.parse(message_user_id), :parse_id},
         {:ok, user} <-
           get_cached_with_fallback(user_id, &UserCache.get/1, &Api.get_user/1),
         {:ok, %{name: channel_name}} <-
           get_cached_with_fallback(channel_id, &ChannelCache.get/1, &Api.get_channel/1),
         {:ok, %{name: guild_name}} <-
           get_cached_with_fallback(guild_id, &GuildCache.get/1, &Api.get_guild/1) do
      Api.create_message(
        channel_id,
        """
        ID #{message_user_id} belongs to: #{User.full_name(user)}
        Message sent in channel: ##{channel_name}
        Channel belongs to guild: #{guild_name}
        """
      )
    else
      # Since we have multiple failure patterns from the combination of `Integer.parse/2`
      # and `is_snowflake/1`, we'll use the identifier as the term to match on instead.
      {_invalid_id, :parse_id} ->
        Api.create_message(channel_id, "Make sure you entered a valid User ID")

      # The cache + API failure case. For now, lets just return the stringified failure
      # reason of whatever the API returned. Up to you if you want to make it all nice and pretty.
      {:error, reason} ->
        Api.create_message(channel_id, "Failed to retrieve all required info: #{inspect(reason)}")
    end
  end

  # Just like handling events in the consumer,
  # a catch-all case for the things we do not care about.
  # The good 'ol "nooper" (yes, as in "noose", you can't stop me)
  def command(_invalid_command), do: :noop
end
