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

  ## Multiple Bots

  Nostrum allows you to start multiple bots under your supervision tree. If you are
  calling a function that interacts with an API specific to a bot - Rest, Cache, Voice, etc. -
  nostrum needs to determine which bot to call it for. It does this with a bot `Registry`
  and process dictionaries so that you don't have to pass an additional argument to every
  function.

  Inside the `handle_event/1` callback in your consumer, the calling process already
  has the context of the bot that generated the event for any API calls that follow.

  If you have a single bot running and need to call an API asynchronously, i.e. *not* as a response
  to a gateway event in your consumer, nostrum will automatically determine the bot.

  However, if you have multiple bots running *and* you need to make unprompted bot calls, you
  will need to assist nostrum by setting the process's context in one of three ways:
  - `Nostrum.Bot.with_bot(MyBot, fn -> ... end)` to wrap the API function calls
  - `use Nostrum.Bot, name: MyBot` at the top of the module where the API-calling functions are defined
  - `Nostrum.Bot.set_bot_name(MyBot)` dynamically before each API call

  You are free to call API functions from newly-spawned tasks as nostrum will search the process dictionaries
  of a few generations of calling processes to find the bot context.

  Note that, unless explicitly configured otherwise (see `t:bot_options/0`), nostrum uses
  the user ID from the bot's token as the bot name. If you wish to dynamically determine
  the user ID from the token, use `Nostrum.Token.decode_token!/1`.
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

  - `:name`: Unique name for your bot. Defaults to the integer bot id encoded into the token.

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

  ## Global config override fields

  The following fields will override global config values if provided.
  More information on globally configuring these options and their defaults
  [may be found here](./intro.html#configuration-options).

  - `:request_guild_members`
  - `:ffmpeg`
  - `:youtubedl`
  - `:streamlink`
  - `:audio_timeout`
  - `:audio_frames_per_burst`
  - `:voice_auto_connect`
  - `:voice_encryption_mode`
  - `:log_full_events`
  - `:log_dispatch_events`
  - `:force_http1`
  """
  @type bot_options :: %{
          required(:consumer) => module(),
          required(:intents) => :all | :nonprivileged | [atom()],
          required(:wrapped_token) => (-> String.t()),
          optional(:name) => name(),
          optional(:shards) =>
            :auto
            | :manual
            | (num_shards :: pos_integer())
            | {lowest :: pos_integer(), highest :: pos_integer(), total :: pos_integer()},
          optional(:request_guild_members) => boolean(),
          optional(:ffmpeg) => String.t(),
          optional(:youtubedl) => String.t(),
          optional(:streamlink) => String.t(),
          optional(:audio_timeout) => pos_integer(),
          optional(:audio_frames_per_burst) => pos_integer(),
          optional(:voice_auto_connect) => boolean(),
          optional(:voice_encryption_mode) => Nostrum.Voice.Crypto.cipher(),
          optional(:log_full_events) => boolean(),
          optional(:log_dispatch_events) => boolean(),
          optional(:force_http1) => boolean()
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

  @doc false
  @doc since: "0.11.0"
  @spec put_default_name(bot_options()) :: bot_options()
  def put_default_name(%{wrapped_token: wrapped_token} = bot_options) do
    # XXX: This should use something like `Plug.Crypto.prune_stacktrace`.
    token = wrapped_token.()
    bot_id = Token.decode_token!(token)
    name = bot_options[:name] || bot_id

    bot_options
    |> Map.put(:name, name)
    |> Map.put(:wrapped_token, fn -> token end)
  end

  # Token is passed wrapped in an anonymous function to prevent exposing it in stacktraces.
  # https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/sensitive_data#wrapping
  @dialyzer nowarn_function: {:init, 1}
  @impl true
  def init(
        {%{consumer: _consumer, wrapped_token: _wrapped_token, name: bot_name} = bot_options,
         supervisor_options}
      ) do
    Util.set_process_label({__MODULE__, bot_name})
    name = {:via, Registry, {Nostrum.Bot.Registry, {:nostrum_bot, bot_name}, bot_options}}

    task_supervisor_name =
      {:via, Registry, {Nostrum.Bot.Registry, {:nostrum_task_supervisor, bot_name}}}

    children = [
      {Nostrum.ConsumerGroup, bot_options},
      {Nostrum.Api.RatelimiterGroup, bot_options},
      {Nostrum.Api.Ratelimiter, bot_options},
      {Nostrum.Store.Supervisor, bot_options},
      {Nostrum.Cache.Supervisor, bot_options},
      {Nostrum.Shard.Supervisor, bot_options},
      {Nostrum.Voice.Supervisor, bot_options},
      {PartitionSupervisor, child_spec: Task.Supervisor, name: task_supervisor_name}
    ]

    Supervisor.start_link(children, supervisor_options ++ [name: name])
  end

  @spec child_spec(options()) :: Supervisor.child_spec()
  def child_spec(%{} = nostrum_options),
    do: child_spec({nostrum_options, [strategy: :one_for_one]})

  def child_spec({nostrum_options, supervisor_options}) do
    nostrum_options = put_default_name(nostrum_options)

    %{
      id: nostrum_options.name,
      start: {__MODULE__, :init, [{nostrum_options, supervisor_options}]},
      type: :supervisor,
      restart: :permanent,
      shutdown: 500
    }
  end

  @bot_fetch_failure_message """
  Unable to find the bot process from the calling process's context. If you are calling
  a bot function outside of a consumer responding to a gateway event or if you have
  more than one bot running, consider one of the following:

  1. Use `#{__MODULE__}.with_bot(bot_name, fn -> ... end)` around your code.
  2. Invoke `use #{__MODULE__}, name: bot_name` in the module where it is being called from.
  3. Call `#{__MODULE__}.set_bot_name(bot_name)` before your API calls

  See the `#{__MODULE__}` documentation for more details.
  """

  @doc """
  Returns all running bots
  """
  @spec fetch_all_bots() :: [%{name: name(), pid: pid(), bot_options: bot_options()}]
  def fetch_all_bots do
    Registry.select(Nostrum.Bot.Registry, [
      {
        {{:nostrum_bot, :"$1"}, :"$2", :"$3"},
        [],
        [%{name: :"$1", pid: :"$2", bot_options: :"$3"}]
      }
    ])
  end

  @doc false
  @spec fetch_bot_pid(name()) :: pid() | nil
  def fetch_bot_pid(name) do
    case Registry.lookup(Nostrum.Bot.Registry, {:nostrum_bot, name}) do
      [{pid, _bot_options}] -> pid
      _ -> nil
    end
  end

  @doc false
  @spec fetch_bot_pid() :: pid()
  def fetch_bot_pid do
    with nil <- get_bot_pid(),
         nil <- fetch_bot_from_name(),
         nil <- fetch_bot_from_all(),
         nil <- fetch_bot_from_callers() do
      raise RuntimeError, @bot_fetch_failure_message
    else
      pid when is_pid(pid) ->
        pid

      %{pid: pid} = bot ->
        set_bot(bot)
        pid
    end
  end

  @doc false
  @spec fetch_bot_name() :: name()
  def fetch_bot_name do
    with nil <- get_bot_name(),
         nil <- fetch_bot_from_all(),
         nil <- fetch_bot_from_callers() do
      raise RuntimeError, @bot_fetch_failure_message
    else
      %{name: name, pid: _} = bot ->
        set_bot(bot)
        name

      name ->
        name
    end
  end

  defp fetch_bot_from_name do
    with name when not is_nil(name) <- get_bot_name(),
         [{pid, bot_options}] <- Registry.lookup(Nostrum.Bot.Registry, {:nostrum_bot, name}) do
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

  # Maximum search depth for calling process ancestry
  @caller_generations 3

  defp fetch_bot_from_callers do
    with [_cal | _lers] = callers <- Process.get(:"$callers"),
         name when not is_nil(name) <-
           callers
           |> Enum.take(@caller_generations)
           |> Enum.find_value(&Process.info(&1)[:dictionary][:nostrum_bot_name]),
         [{pid, bot_options}] <- Registry.lookup(Nostrum.Bot.Registry, {:nostrum_bot, name}) do
      %{pid: pid, bot_options: bot_options, name: name}
    else
      _ -> nil
    end
  end

  @doc false
  def get_bot_name, do: Process.get(:nostrum_bot_name)
  @doc false
  def get_bot_pid, do: Process.get(:nostrum_bot_pid)

  @doc """
  Manually set the bot name in the calling process's context
  """
  @spec set_bot_name(name()) :: name() | nil
  def set_bot_name(name), do: Process.put(:nostrum_bot_name, name)
  @doc false
  def set_bot_pid(pid), do: Process.put(:nostrum_bot_pid, pid)

  defp set_bot(%{pid: pid, bot_options: _bot_options, name: name}), do: set_bot(pid, name)
  defp set_bot(pid, name), do: {set_bot_pid(pid), set_bot_name(name)}

  def spawn_task(
        task_func,
        bot_name \\ fetch_bot_name(),
        task_key \\ :erlang.unique_integer()
      ) do
    # Apparently, PartitionSupervisors support a second level of
    # nesting `:via` lookups.
    supervisor_name =
      {:via, Registry, {Nostrum.Bot.Registry, {:nostrum_task_supervisor, bot_name}}}

    Task.Supervisor.start_child(
      {:via, PartitionSupervisor, {supervisor_name, task_key}},
      task_func
    )
  end

  @doc """
  Call a function with the context of the provided bot

  ```elixir
  Nostrum.Bot.with_bot(ReticentBot, fn ->
    Nostrum.Api.Message.create(channel_id, "Hi...")
  end)

  Nostrum.Bot.with_bot(SillyGoofyBot, fn ->
    Nostrum.Api.Message.create(channel_id, "Heyyy :)")
  end, :keep)

  # Context is still set to SillyGoofyBot
  Nostrum.Api.Message.create(channel_id, "Me again :)")
  ```

  ## Parameters

  - `name` - Name of the bot to run the function as
  - `function` - Zero arity function to execute
  - `next` - What to do with the process's bot context after the function
    - `:reset` - Reset the values to what they were before (default)
    - `:clear` - Set the values to `nil`
    - `:keep` - Keep the provided bot's context in this process's dictionary
  """
  @spec with_bot(name(), (-> any()), :reset | :clear | :keep) :: any()
  def with_bot(name, function, next \\ :reset) do
    {prev_pid, prev_name} = set_bot(fetch_bot_pid(name), name)

    try do
      function.()
    after
      case next do
        :reset -> set_bot(prev_pid, prev_name)
        :clear -> set_bot(nil, nil)
        :keep -> :noop
      end
    end
  end

  defmacro __using__(opts) do
    bot_name = opts[:name] || raise "Must define name when invoking `use #{__MODULE__}`"

    quote do
      import Kernel, except: [def: 2]
      import Nostrum.Bot.Macros
      defp __name__, do: unquote(bot_name)
    end
  end

  defmodule Macros do
    @moduledoc false
    import Kernel, except: [def: 2]
    alias Nostrum.Bot

    defmacro def(call, do: body) do
      quote do
        Kernel.def unquote(call) do
          Bot.get_bot_name() || Bot.set_bot_name(__name__())
          unquote(body)
        end
      end
    end
  end
end
