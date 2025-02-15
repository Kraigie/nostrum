defmodule Nostrum.Bot do
  @moduledoc """
  Supervisor for a bot along with all its associated components.

  ## Usage

  In your supervisor tree, write:

  ```elixir
  bot_options = %{
    consumer: MyBot.Consumer,
    # This is an example and includes privileged intents, but
    # should provide a useful set of intents for bots. Feel
    # free to adjust this as you see fit.
    intents: [
      :direct_messages,
      :guild_bans,
      :guild_members,
      :guild_message_reactions,
      :guild_messages,
      :guilds,
      :message_content
    ],
    wrapped_token: fn -> System.fetch_env!("BOT_TOKEN") end,
  }
  children = [
    {Nostrum.Bot, {bot_options, supervisor_options}}
  ]
  ```

  See `t:bot_options/0` for which options you can configure here.
  `t:supervisor_options/0` allow you to customize the options we start the
  `Supervisor` with.

  ## Migration guide

  If you are still using the old way of deploying nostrum, please do the following:

  1. Remove the `:nostrum`, `:token` configuration option. nostrum will then
  assume that you are starting it as part of your own supervision tree, and
  automatic startup via the application is disabled.

  2. Remove your consumer from your application supervisor tree.

  3. In your supervisor tree, add the `bot_options` described above, and add
  `Nostrum.Bot` to your supervisor tree as described above. For the `:intents`
  setting, cut the value you previously had in your `config.exs` and use it
  here. You can then remove the config value.

  4. In your consumer, change `use Nostrum.Consumer` to `@behaviour Nostrum.Consumer`.

  5. In your consumer, add a default event handling clause for unmatched events
  at the end of the module, such as:

  ```elixir
    def handle_event(_), do: :ok
  ```

  ## Limitations

  This module was introduced in version `0.11.0` to provide a first step
  towards migrating away from nostrum starting as an application. However, at
  present, you are not able to start multiple bots with this at the same time
  due to global names used internally. However, as we migrate away from the
  application-based deployment, further internal changes should not require any
  update from you.
  """
  # TODO: document dynamic starting and multiple shards
  # TODO(v0.12.0 and above): Remove migration guide
  @moduledoc since: "0.11.0"

  @behaviour Supervisor

  alias Nostrum.Token
  alias Nostrum.Util

  @typedoc """
  Options to start a bot with.

  ## Required fields

  - `:consumer`: A module implementing the `Nostrum.Consumer` behaviour that
  should be called for each event.

  - `:intents`: Which gateway intents to request. See the [Gateway
  Intents](gateway_intents.html) documentation for more details.

  - `:wrapped_token`: A function that takes no arguments and returns the bot
  token to use. This is wrapped to prevent exposure of the token in
  stacktraces.
  """
  @type bot_options :: %{
          required(:consumer) => module(),
          required(:wrapped_token) => (-> String.t()),
          required(:intents) => :all | :nonprivileged | [atom()]
        }

  @typedoc """
  Which gateway intents to request.

  A full reference can be found either [on
  Discord](https://discord.com/developers/docs/events/gateway#gateway-intents)
  or on our [Gateway Intents](gateway_intents.html) documentation.
  """
  @type intents :: :all | :nonprivileged | [atom()]

  @typedoc """
  Options to pass to the bot's supervisor.

  By default, we set the `strategy: :one_for_one`. No other options are set by
  nostrum.
  """
  @type supervisor_options :: [Supervisor.init_option()]

  @typedoc false
  @type options :: {bot_options(), supervisor_options()}

  # Token is passed wrapped in an anonymous function to prevent exposing it in stacktraces.
  # https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/sensitive_data#wrapping
  @dialyzer nowarn_function: {:init, 1}
  @impl true
  def init(
        {%{consumer: _consumer, wrapped_token: wrapped_token} = bot_options, supervisor_options}
      ) do
    user_id = Token.check_token!(wrapped_token.())
    name = {__MODULE__, user_id}
    Util.set_process_label(name)

    children = [
      Nostrum.Store.Supervisor,
      Nostrum.ConsumerGroup,
      Nostrum.Api.RatelimiterGroup,
      {Nostrum.Api.Ratelimiter, {wrapped_token, []}},
      Nostrum.Shard.Connector,
      Nostrum.Cache.CacheSupervisor,
      {Nostrum.Shard.Supervisor, {bot_options, []}},
      Nostrum.Voice.Supervisor
    ]

    Supervisor.start_link(children, supervisor_options)
  end

  @spec child_spec(options()) :: map()
  def child_spec({nostrum_options, supervisor_options}) do
    supervisor_options = Keyword.put_new(supervisor_options, :strategy, :one_for_one)

    %{
      id: __MODULE__,
      start: {__MODULE__, :init, [{nostrum_options, supervisor_options}]},
      type: :supervisor,
      restart: :permanent,
      shutdown: 500
    }
  end
end
