defmodule Nostrum.Voice.Ports do
  @moduledoc false

  defmodule State do
    @moduledoc false

    defstruct [
      :q,
      :port,
      :port_done,
      :awaiter,
      :input_pid
    ]

    def new(port, input_pid) do
      %__MODULE__{
        q: :queue.new(),
        port_done: false,
        port: port,
        input_pid: input_pid
      }
    end
  end

  @dialyzer {:nowarn_function, init: 1}

  alias Nostrum.Voice.Ports.State

  require Logger

  use GenServer

  def init({executable, args, input}) do
    port =
      Port.open({:spawn_executable, executable}, [
        {:args, args},
        :binary,
        :exit_status,
        :use_stdio,
        :stream
      ])

    # Spawn process to asynchronously send input to port
    if input, do: spawn(fn -> send_input(port, input) end)

    # Store reference if input is another process
    input_pid = if is_pid(input), do: input, else: nil

    {:ok, State.new(port, input_pid)}
  end

  @spec execute(String.t(), [String.t()], iodata() | pid() | nil) ::
          {:ok, pid()} | {:error, String.t()}
  def execute(executable, args \\ [], input \\ nil) do
    case System.find_executable(executable) do
      nil ->
        {:error, "#{executable} not found"}

      path ->
        {:ok, _pid} = GenServer.start(__MODULE__, {path, args, input})
    end
  end

  # Input is another Ports process
  defp send_input(port, input) when is_pid(input),
    do: Enum.each(get_stream(input), &send_input(port, &1))

  # Input is iodata
  defp send_input(port, input) do
    Port.command(port, input)
  catch
    :error, :badarg -> :error
  end

  @spec get_stream(pid()) :: Enum.t()
  def get_stream(pid) do
    Stream.unfold(pid, fn pid ->
      case get(pid) do
        nil -> nil
        data -> {data, pid}
      end
    end)
  end

  def get(pid) do
    if Process.alive?(pid),
      do: GenServer.call(pid, :get, :infinity)
  end

  @spec close(pid()) :: no_return()
  def close(pid) do
    if Process.alive?(pid),
      do: GenServer.cast(pid, :close)
  end

  @empty_queue {[], []}

  def handle_call(:get, _from, %{q: @empty_queue, port_done: true}) do
    {:stop, :shutdown, nil, nil}
  end

  def handle_call(:get, _from, %{q: q, port_done: true} = state) do
    {:reply, :queue.head(q), %{state | q: :queue.tail(q)}}
  end

  def handle_call(:get, from, %{q: @empty_queue} = state) do
    {:noreply, %{state | awaiter: from}}
  end

  def handle_call(:get, _from, %{q: q} = state) do
    {:reply, :queue.head(q), %{state | q: :queue.tail(q)}}
  end

  def handle_cast(:close, %{awaiter: awaiter, port: port, input_pid: input_pid}) do
    if awaiter, do: GenServer.reply(awaiter, nil)
    if is_pid(input_pid), do: close(input_pid)
    Logger.debug("Closing port #{inspect(port)}")

    # Safely try to close the port
    try do
      Port.close(port)
    rescue
      ArgumentError -> :noop
    end

    {:stop, :shutdown, nil}
  end

  def handle_info({_port, {:data, data}}, %{q: q, awaiter: nil} = state) do
    {:noreply, %{state | q: :queue.in(data, q)}}
  end

  def handle_info({_port, {:data, data}}, %{awaiter: awaiter} = state) do
    GenServer.reply(awaiter, data)
    {:noreply, %{state | awaiter: nil}}
  end

  def handle_info({_port, {:exit_status, _status}}, %{awaiter: nil} = state) do
    {:noreply, %{state | port_done: true}}
  end

  def handle_info({_port, {:exit_status, _status}}, %{q: @empty_queue, awaiter: awaiter}) do
    GenServer.reply(awaiter, nil)
    {:stop, :shutdown, nil}
  end

  def handle_info({_port, {:exit_status, _status}}, %{q: q, awaiter: awaiter} = state) do
    GenServer.reply(awaiter, :queue.head(q))
    {:noreply, %{state | port_done: true, awaiter: nil}}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def code_change(_version, state, data, _extra) do
    {:ok, state, data}
  end
end
