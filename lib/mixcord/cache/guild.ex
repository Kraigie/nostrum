defmodule Mixcord.Cache.Guild do
  @moduledoc """
  """

  use GenServer

  # TODO: Length\Update?
  # TODO: https://www.djm.org.uk/posts/elixir-keyword-lists-function-parameters/

  def start_link do
    GenServer.start_link(__MODULE__, [], name: Guilds)
  end

  def all() do
    GenServer.call(Guilds, :all)
  end

  def get(id: id), do: GenServer.call(Guilds, {:get, id: id})
  def get(channel_id: channel_id), do: GenServer.call(Guilds, {:get, channel: channel_id})

  def get!(id: id) do
    get(id: id)
      |> bangify_find
  end

  def get!(channel_id: channel_id) do
    get(channel_id: channel_id)
      |> bangify_find
  end

  def search(fun), do: GenServer.call(Guilds, {:search, fun})
  def search!(fun) do
    GenServer.call(Guilds, {:search, fun})
    |> bangify_find
  end

  @doc false
  def create(guild) do
    GenServer.cast(Guilds, {:create, guild})
  end

  @doc false
  def remove(id: id) do
    GenServer.cast(Guilds, {:remove, id: id})
  end

  def handle_call(:all, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:get, id: id}, _from, state) do
    {:reply, Enum.find(state, fn guild -> guild.id == id end), state}
  end

  def handle_call({:get, channel_id: channel_id}, _from, state) do
    {:reply, Enum.find(state, fn guild -> guild.id == channel_id end), state}
  end

  def handle_call({:search, fun}, _from, state) do
    {:reply, Enum.find(state, fun), state}
  end

  def handle_cast({:create, guild}, state) do
    {:noreply, state ++ guild}
  end

  def handle_cast({:remove, id}, state) do
    {:noreply, Enum.filter(state, fn guild -> guild.id == id end)}
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