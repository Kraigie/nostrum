defmodule Mixcord.Cache.Channel do
  @moduledoc """
  Cache for channels.
  """

  use GenServer
  alias Mixcord.Util

  @type channel :: Mixcord.Map.TextChannel.t | Mixcord.Map.VoiceChannel.t

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: Channels)
  end

  def init(state) do
    {:ok, state}
  end

  @spec get(id: integer) :: channel
  @spec get(message: Mixcord.Map.Message.t) :: channel
  def get(id: id), do: GenServer.call(Channels, {:get, id})
  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find
  end

  @doc false
  def create(channel), do: GenServer.call(Channels, {:create, channel.id, channel})

  @doc false
  def update(channel), do: GenServer.call(Channels, {:update, channel.id, channel})

  @doc false
  def delete(channel), do: GenServer.call(Channels, {:delete, channel.id})

  # I'm willing to abuse `from` here to provide `async` searching if this turns out to be slow
  def handle_call({:get, id}, _from, state) do
    {:reply, Map.get(state, id), state}
  end

  def handle_call({:create, id, channel}, _from, state) do
    {:reply, channel, Map.put(state, id, channel)}
  end

  def handle_call({:update, id, channel}, _from, state) do
    {old_channel, new_state} = Map.pop(state, id)
    {:reply, {old_channel, channel}, Map.put(new_state, id, channel)}
  end

  def handle_call({:delete, id}, _from, state) do
    {old_channel, new_state} = Map.pop(state, id)
    {:reply, old_channel, new_state}
  end

end