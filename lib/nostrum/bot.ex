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
    {Nostrum.Bot, bot_options}
  ]
  ```

  See `t:bot_options/0` for which options you can configure here.
  You may also configure `t:supervisor_options/0`, which allow you
  to customize the options we start the `Supervisor` with by defining
  your child in the form `{Nostrum.Bot, {bot_options, supervisor_options}}`,
  but the default `t:supervisor_options/0` should suit most needs.

  ## Migration guide

  If you are still using the old way of deploying nostrum, please do the following:

  1. Remove the `:nostrum`, `:token` configuration option. nostrum will then
  assume that you are starting it as part of your own supervision tree, and
  automatic startup via the application is disabled.

  2. Remove your consumer from your application supervisor tree.

  3. In your supervisor tree, add the `bot_options` described above, and add
  `Nostrum.Bot` to your supervisor tree as described above. For the `:intents`
  setting, cut the value you previously had in your `config.exs` and use it
  here. Do the same with `:num_shards`, renaming it to `:shards` on the
  `bot_options` map. You can then remove the old config values.

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

  ## Optional fields

  - `:shards`: Shards that should be started with this bot. Possible values:

    - `:auto` uses the suggested amount of shards as provided by Discord. This
    value is the default.

    - `:manual` do not automatically spawn shards. In this case, it is your
    responsibility to spawn shards manually, see [the manual sharding
    documentation](./manual_sharding.html).

    - `pos_integer()`: A number of shards to run. nostrum will warn if this is
    not the recommended amount.

    - `{lowest, highest, total}` starts shards `lowest` to `highest`. `total`
    should contain the total amount of shards that your bot is expected to
    have. Useful for splitting a single bot across multiple nodes, see the
    [multi-node documentation](./multi_node.html) for further information.

  - `:name`: Unique name for your bot. Defaults to the bot_id encoded into the token.
  """
  @type bot_options :: %{
          required(:consumer) => module(),
          required(:intents) => :all | :nonprivileged | [atom()],
          required(:wrapped_token) => (-> String.t()),
          optional(:name) => name(),
          optional(:shards) =>
            :auto
            | :manual
            | (num_shards ::
                 pos_integer())
            | {lowest :: pos_integer(), highest :: pos_integer(), total :: pos_integer()}
        }

  @typedoc """
  Which gateway intents to request.

  A full reference can be found either [on
  Discord](https://discord.com/developers/docs/events/gateway#gateway-intents)
  or on our [Gateway Intents](gateway_intents.html) documentation.
  """
  @type intents :: :all | :nonprivileged | [atom()]

  @typedoc """
  Unique name for the bot.
  """
  @type name :: atom() | integer() | String.t()

  @typedoc """
  Options to pass to the bot's supervisor.

  By default, we set the `strategy: :one_for_one`. No other options are set by
  nostrum.
  """
  @type supervisor_options :: [Supervisor.init_option()]

  @typedoc false
  @type options :: bot_options() | {bot_options(), supervisor_options()}

  # Token is passed wrapped in an anonymous function to prevent exposing it in stacktraces.
  # https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/sensitive_data#wrapping
  @dialyzer nowarn_function: {:init, 1}
  @impl true
  def init(
        {%{consumer: _consumer, wrapped_token: wrapped_token} = bot_options, supervisor_options}
      ) do
    token = wrapped_token.()
    bot_id = Token.decode_token!(token)
    name = bot_options[:name] || bot_id

    bot_options =
      bot_options
      |> Map.put(:name, name)
      |> Map.put(:wrapped_token, fn -> token end)

    Util.set_process_label({__MODULE__, name})
    name = {:via, Registry, {Nostrum.Bot.Registry, name, bot_options}}

    children = [
      {Nostrum.ConsumerGroup, bot_options},
      {Nostrum.Api.RatelimiterGroup, bot_options},
      {Nostrum.Api.Ratelimiter, bot_options},
      {Nostrum.Store.Supervisor, bot_options},
      {Nostrum.Cache.Supervisor, bot_options},
      {Nostrum.Shard.Supervisor, bot_options},
      {Nostrum.Voice.Supervisor, bot_options}
    ]

    Supervisor.start_link(children, supervisor_options ++ [name: name])
  end

  @spec child_spec(options()) :: Supervisor.child_spec()
  def child_spec(%{} = nostrum_options),
    do: child_spec({nostrum_options, [strategy: :one_for_one]})

  def child_spec({nostrum_options, supervisor_options}) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :init, [{nostrum_options, supervisor_options}]},
      type: :supervisor,
      restart: :permanent,
      shutdown: 500
    }
  end

  defmacro __using__(opts) do
    bot_name = opts[:name] || raise "Must define name when invoking `use #{__MODULE__}`"

    quote do
      defp name, do: unquote(bot_name)
    end
  end

  @bot_fetch_failure_message """
  Unable to find the bot process from the calling process's context. If you are calling
  a bot function outside of a consumer responding to a gateway event or if you have
  more than one bot running, consider invoking `use #{__MODULE__}, name: {name}` in
  the module where it is being called from.
  """

  @spec fetch_all_bots() :: [%{name: name(), pid: pid(), bot_options: bot_options()}]
  def fetch_all_bots do
    Registry.select(Nostrum.Bot.Registry, [
      {{:"$1", :"$2", :"$3"}, [], [%{name: :"$1", pid: :"$2", bot_options: :"$3"}]}
    ])
  end

  @spec fetch_bot_pid(name()) :: pid() | nil
  def fetch_bot_pid(name) do
    case Registry.lookup(Nostrum.Bot.Registry, name) do
      [{pid, _bot_options}] -> pid
      _ -> nil
    end
  end

  @spec fetch_bot_pid() :: pid()
  def fetch_bot_pid do
    with nil <- get_bot_pid(),
         nil <- fetch_bot_from_name(),
         nil <- fetch_bot_from_options(),
         nil <- fetch_bot_from_all() do
      raise RuntimeError, @bot_fetch_failure_message
    else
      pid when is_pid(pid) ->
        pid

      %{pid: pid} = bot ->
        set_bot(bot)
        pid
    end
  end

  @spec fetch_bot_name() :: name()
  def fetch_bot_name do
    with nil <- get_bot_name(),
         nil <- get_bot_options(),
         nil <- fetch_bot_from_name(),
         nil <- fetch_bot_from_options(),
         nil <- fetch_bot_from_all() do
      raise RuntimeError, @bot_fetch_failure_message
    else
      %{name: name, consumer: _} = _bot_options ->
        name

      %{name: name, pid: _} = bot ->
        set_bot(bot)
        name

      name ->
        name
    end
  end

  defp fetch_bot_from_name do
    with name when not is_nil(name) <- get_bot_name(),
         [{pid, bot_options}] <- Registry.lookup(Nostrum.Bot.Registry, name) do
      %{pid: pid, bot_options: bot_options, name: name}
    else
      _ -> nil
    end
  end

  defp fetch_bot_from_options do
    with %{name: name} <- get_bot_options(),
         [{pid, bot_options}] <- Registry.lookup(Nostrum.Bot.Registry, name) do
      %{pid: pid, bot_options: bot_options, name: name}
    else
      _ -> nil
    end
  end

  defp fetch_bot_from_all do
    case fetch_all_bots() do
      [%{pid: _, bot_options: _, name: _} = bot] -> bot
      _zero_or_multiple_bots -> nil
    end
  end

  def get_bot_options, do: Process.get(:nostrum_bot_options)
  def get_bot_name, do: Process.get(:nostrum_bot_name)
  def get_bot_pid, do: Process.get(:nostrum_bot_pid)

  def set_bot_options(options), do: Process.put(:nostrum_bot_options, options)
  def set_bot_name(name), do: Process.put(:nostrum_bot_name, name)
  def set_bot_pid(pid), do: Process.put(:nostrum_bot_pid, pid)

  defp set_bot(%{pid: pid, bot_options: bot_options, name: name}) do
    set_bot_pid(pid)
    set_bot_options(bot_options)
    set_bot_name(name)
  end
end
