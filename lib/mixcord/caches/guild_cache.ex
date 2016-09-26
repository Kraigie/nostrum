defmodule Mixcord.Caches.Guilds do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], name: Guilds)
  end

  def all() do
    GenServer.call(Guilds, :all)
  end

  def lookup(id) do
    GenServer.call(Guilds, {:lookup, id})
  end
  #TODO: Bangify
  def lookup!(id) do
    GenServer.call(Guilds, {:lookup, id})
  end

  def search(fun) do
    GenServer.call(Guilds, {:search, fun})
  end

  def create(guild) do
    GenServer.call(Guilds, {:create, guild})
  end

  def remove(guild) do
    GenServer.call(Guilds, {:remove, guild})
  end

  def init(_args) do
    {:ok, []}
  end

  def handle_call(:all, _from, state) do
    {:reply, state}
  end

  def handle_call({:lookup, id}, _from, state) do
    {:reply, Enum.find(state, fn guild -> guild.id == id end)}
  end

  def handle_call({:search, fun}, _from, state) do
    {:reply, Enum.find(state, fun)}
  end

end