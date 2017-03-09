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

  @doc ~S"""
  Retrieves a channel from the cache.
  """
  # TODO: Change to DM only cache or handle getting channels from guild processes.
  @spec get(id: integer | Nostrum.Struct.Message.t) :: channel
  def get(id: id), do: GenServer.call(ChannelCache, {:get, id})
  def get(%Nostrum.Struct.Message{channel_id: channel_id}), do: get(id: channel_id)

  def get!(id: id) do
    get(id: id)
    |> Util.bangify_find(id, __MODULE__)
  end

  @doc false
  def create(channel),
    do: GenServer.call(ChannelCache, {:create, channel.id, channel})

  @doc false
  def update(channel),
    do: GenServer.call(ChannelCache, {:update, channel.id, channel})

  @doc false
  def delete(channel),
    do: GenServer.call(ChannelCache, {:delete, channel.id})

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
