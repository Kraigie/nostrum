defmodule Nostrum.Cache.UserCache.NoOp do
  @moduledoc """
  A NoOp implementation for the UserCache

  This cache does nothing, enable it if you dont need to cache users
  """
  @behaviour Nostrum.Cache.UserCache

  alias Nostrum.Struct.User
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl Nostrum.Cache.UserCache
  def bulk_create(_users), do: :ok

  @impl Nostrum.Cache.UserCache
  def create(payload), do: User.to_struct(payload)

  @impl Nostrum.Cache.UserCache
  def update(info), do: {nil, User.to_struct(info)}

  @impl Nostrum.Cache.UserCache
  def delete(_id), do: :noop

  @impl Nostrum.Cache.UserCache
  def query_handle, do: :qlc.string_to_handle('[].')
end
