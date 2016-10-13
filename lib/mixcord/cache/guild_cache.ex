defmodule Mixcord.Cache.Guilds do
  @moduledoc """
  """

  use GenServer

  #TODO: Length\Update?

  def start_link do
    GenServer.start_link(__MODULE__, [], name: Guilds)
  end

  def init(_args) do
    {:ok, []}
  end

  def all() do
    GenServer.call(Guilds, :all)
  end

  def lookup(id) do
    GenServer.call(Guilds, {:lookup, id})
  end

  def lookup!(id) do
    GenServer.call(Guilds, {:lookup, id})
    |> bangify_find
  end

  def search(fun) do
    GenServer.call(Guilds, {:search, fun})
  end

  def search!(fun) do
    GenServer.call(Guilds, {:search, fun})
    |> bangify_find
  end

  @doc false
  def create!(guild) do
    GenServer.cast(Guilds, {:create, guild})
  end

  @doc false
  def remove!(guild) do
    GenServer.cast(Guilds, {:remove, guild})
  end

  def handle_call(:all, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:lookup, id}, _from, state) do
    {:reply, Enum.find(state, fn guild -> guild.id == id end), state}
  end

  def handle_call({:search, fun}, _from, state) do
    {:reply, Enum.find(state, fun), state}
  end

  def handle_cast({:create, guild}, state) do
    {:noreply, state ++ guild}
  end

  def handle_cast({:remove, _guild}, state) do
    {:noreply, state}
  end

  def bangify_find(to_bang) do
    case to_bang do
      nil ->
        raise(Mixcord.Error.CacheError)
      ret ->
        ret
    end
  end

end