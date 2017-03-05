defmodule Nostrum.Cache.ChannelCache do
  @moduledoc """
  Cache for ChannelCache.
  """

  use GenServer

  alias Nostrum.Util

  @type channel :: Nostrum.Struct.Channel.TextChannel.t | Nostrum.Struct.Channel.VoiceChannel.t

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: ChannelCache)
  end

  def init(state) do
    {:ok, state}
  end

  @spec get(id: integer) :: channel
  @spec get(message: Nostrum.Struct.Message.t) :: channel
  def get(id: id), do: GenServer.call(ChannelCache, {:get, id})
  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find(id, __MODULE__)
  end

  @doc false
  def create(channel),
    do: {:ok, GenServer.call(ChannelCache, {:create, channel.id, channel})}

  @doc false
  def update(channel),
    do: {:ok, GenServer.call(ChannelCache, {:update, channel.id, channel})}

  @doc false
  def delete(channel),
    do: {:ok, GenServer.call(ChannelCache, {:delete, channel.id})}

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
