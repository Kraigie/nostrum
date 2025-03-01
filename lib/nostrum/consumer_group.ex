defmodule Nostrum.ConsumerGroup do
  @moduledoc """
  Registers consumers and handles event dispatch.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Bot
  alias Nostrum.Consumer

  @base_scope_name :nostrum_consumer_group
  @group_name :consumers

  @doc """
  Monitor the consumer group for changes.

  Return the initial state of the group on first call. Further updates are
  delivered as messages to the calling process, see `:pg.monitor/2` for
  details. The returned `t:reference/0` must be saved for later calls to
  `demonitor/1`.
  """
  @doc since: "0.9.0"
  @spec monitor :: {reference(), [pid()]}
  def monitor do
    :pg.monitor(scope_name(), @group_name)
  end

  @doc """
  Stop monitoring the given reference.
  """
  @doc since: "0.9.0"
  @spec demonitor(reference()) :: :ok | false
  def demonitor(ref) do
    :pg.demonitor(scope_name(), ref)
  end

  @doc """
  Dispatch the given event(s) to all consumers.

  This is called by nostrum internally, you likely won't need to call this
  manually.
  """
  @spec dispatch(nonempty_list(Consumer.event()), Bot.name()) :: :ok
  @spec dispatch(Consumer.event(), Bot.name()) :: :ok
  def dispatch([:noop | events], bot_name), do: dispatch(events, bot_name)

  def dispatch([event | events], bot_name) do
    payload = {:event, event}

    bot_name
    |> scope_name()
    |> :pg.get_members(@group_name)
    |> Enum.each(&send(&1, payload))

    dispatch(events, bot_name)
  end

  def dispatch(:noop, _), do: :ok
  def dispatch([], _), do: :ok
  def dispatch(event, bot_name) when is_tuple(event), do: dispatch([event], bot_name)

  @doc """
  Equivalent to `ConsumerGroup.join(self())`. See `join/1`.
  """
  @spec join :: :ok
  def join, do: join(self())

  @doc """
  Join the given process to the consumers.

  If no process is given, joins the current process to the consumers. This can
  be used for subscribing to gateway events and awaiting them inline.

  After the process has joined, it will receive any events sent by nostrum's
  gateway dispatch. These events are sent as messages `{:event,
  t:Consumer.Event.t/0}`. The given `pid` is automatically unsubscribed when it
  terminates.

  Note that there is currently no filtering done. If the gateway sends a lot of
  messages and the event subscriber does not terminate swiftly, its message
  queue will keep growing.

  ## Example

  The following example illustrates how to use this to implement inline event
  awaiting:

  ```elixir
  defmodule MyBot.Command
    alias Nostrum.Api.Message
    alias Nostrum.ConsumerGroup
    alias Nostrum.Struct.Message
    alias Nostrum.Struct.User

    def command(%Message{author: %User{id: author_id}}) do
      Message.create(msg, "Reply 'y' in 5 seconds to confirm ordering a large burger menu.")
      ConsumerGroup.join()
      receive do
        {:event, {:MESSAGE_CREATE, %Message{author: %User{id: author_id}, content: "y"}, _}} ->
          Message.create(msg, "The large burger menu is coming.")
      after
        5_000 ->
          Message.create(msg, "Too slow!")
      end
    end
  end
  ```
  """
  @spec join(pid()) :: :ok
  def join(pid) do
    :pg.join(scope_name(), @group_name, pid)
  end

  @doc "Equivalent to `ConsumerGroup.leave(self())`. See `leave/1`."
  @doc since: "0.11.0"
  def leave do
    leave(self())
  end

  @doc """
  Leave the given process or processes from the current bot's consumer group.

  Useful if you wish to stop listening for events after a while.
  """
  @doc since: "0.11.0"
  @spec leave(pid() | [pid()]) :: :ok | :not_joined
  def leave(pid_or_pids) do
    :pg.leave(scope_name(), @group_name, pid_or_pids)
  end

  def start_link(%{name: bot_name} = _opts) do
    :pg.start_link(scope_name(bot_name))
  end

  defp scope_name(bot_name \\ Bot.fetch_bot_name()), do: :"#{@base_scope_name}_#{bot_name}"

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end
end
