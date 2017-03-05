defmodule Nostrum.Shard.Dispatch.ProducerSupervisor do
  @moduledoc false

  use Supervisor
  
  alias Nostrum.Shard.Dispatch.Producer

  def start_link do
    Supervisor.start_link(__MODULE__, [])
  end

  def start_producer(pid, id) do
    Supervisor.start_child(pid, worker(Producer, [id]))
  end

  @doc false
  def init(_) do
    supervise([], strategy: :one_for_one)
  end
end
