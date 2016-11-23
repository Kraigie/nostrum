defmodule Mixcord.Cache.User do
  @moduledoc """
  Cache for channels.

  The ETS table name associated with the Channel User is `:users`. Besides the
  methods provided below you can call any other ETS methods on the table.

  ## Example
  ```elixir
  info = :ets.info(:users)
  [..., heir: :none, name: :users, size: 1, ...]
  size = info[:size]
  1
  ```
  """

  use GenServer
  alias Mixcord.Util

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, [], name: Users)
  end

  def init(_args) do
    :ets.new(:users, [:set, :public, :named_table])
    {:ok, []}
  end

  @spec get(id: integer) :: Mixcord.Map.User.t
  @spec get(message: Mixcord.Map.Message.t) :: Mixcord.Map.User.t
  def get(id: id), do: :ets.lookup_element(:users, id, 2)
  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find
  end


  @doc false
  def create(user), do: GenServer.cast(Users, {:create, user.id, user: user})

  @doc false
  def update(user), do: GenServer.cast(Users, {:update, user.id, user: user})

  @doc false
  def delete(user), do: GenServer.cast(Users, {:delete, user_id: user.id})

  def handle_cast({:create, user_id, user: user}, state) do
    :ets.insert(:users, {user_id, user})
    {:noreply, state}
  end

  def handle_cast({:update, user_id, user: user}, state) do
    :ets.update_element(:users, user_id, user)
    {:noreply, state}
  end

  def handle_cast({:delete, user_id: id}, state) do
    :ets.delete(:users, id)
    {:noreply, state}
  end

end