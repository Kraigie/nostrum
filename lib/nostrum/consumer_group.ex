defmodule Nostrum.ConsumerGroup do
  @moduledoc """
  Registers consumers and handles event dispatch.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Consumer

  @scope_name __MODULE__
  @group_name :consumers

  @doc """
  Dispatch the given event(s) to all consumers.

  This is called by nostrum internally, you likely won't need to call this
  manually.
  """
  @spec dispatch(nonempty_list(Consumer.event())) :: :ok
  @spec dispatch(Consumer.event()) :: :ok
  def dispatch([:noop | events]) do
    dispatch(events)
  end

  def dispatch([event | events]) do
    payload = {:event, event}

    @scope_name
    |> :pg.get_members(@group_name)
    |> Enum.each(&GenServer.cast(&1, payload))

    dispatch(events)
  end

  def dispatch([]), do: :ok
  def dispatch(event) when is_tuple(event), do: dispatch([event])
  def dispatch(:noop), do: :ok

  @doc """
  Join the given process to the consumers.

  After the process has joined, it will receive any events sent by nostrum's
  gateway dispatch.
  """
  def join(pid) do
    :pg.join(@scope_name, @group_name, pid)
  end

  def start_link(_opts) do
    :pg.start_link(@scope_name)
  end

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
