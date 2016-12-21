defmodule Mixcord.Cache.User do
  @moduledoc """
  Cache for channels.
  """

  use GenServer
  alias Mixcord.Util

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: Users)
  end

  def init(state) do
    {:ok, state}
  end

  @spec get(id: integer) :: Mixcord.Map.User.t
  @spec get(message: Mixcord.Map.Message.t) :: Mixcord.Map.User.t
  def get(id: id), do: GenServer.call(Users, {:get, id})
  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find(id, __MODULE__)
  end


  @doc false
  def create(user), do: GenServer.call(Users, {:create, user.id, user})

  @doc false
  def update(user), do: GenServer.call(Users, {:update, user.id, user})

  @doc false
  def delete(user), do: GenServer.call(Users, {:delete, user.id})

  def handle_call({:get, id}, _from, state) do
    {:reply, Map.get(state, id), state}
  end

  def handle_call({:create, id, user}, _from, state) do
    {:reply, user, Map.put(state, id, user)}
  end

  def handle_call({:update, id, user}, _from, state) do
    {old_user, new_state} = Map.pop(state, id)
    {:reply, {old_user, user}, Map.put(new_state, id, user)}
  end

  def handle_call({:delete, id}, _from, state) do
    {old_channel, new_state} = Map.pop(state, id)
    {:reply, old_channel, new_state}
  end

end