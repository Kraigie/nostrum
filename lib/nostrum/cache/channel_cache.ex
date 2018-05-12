defmodule Nostrum.Cache.ChannelCache do
  @moduledoc """
  Cache for ChannelCache.
  """

  use GenServer

  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Channel
  alias Nostrum.Util

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: ChannelCache)
  end

  def init([]) do
    {:ok, %{}}
  end

  @doc ~S"""
  Retrieves a channel from the cache.

  Internally, the ChannelCache process only stores
  `t:Nostrum.Struct.Channel.dm_channel/0` references. To get channel
  information, a call is made to a `Nostrum.Cache.GuildCache`.
  """
  @spec get(id: integer | Nostrum.Struct.Message.t()) :: {:error, atom} | {:ok, Channel.t()}
  def get(id: id), do: GenServer.call(ChannelCache, {:get, id})
  def get(%Nostrum.Struct.Message{channel_id: channel_id}), do: get(id: channel_id)

  def get!(id: id) do
    get(id: id)
    |> Util.bangify_find(id, __MODULE__)
  end

  @doc false
  def create(channel), do: GenServer.call(ChannelCache, {:create, channel.id, channel})

  @doc false
  def update(channel), do: GenServer.call(ChannelCache, {:update, channel.id, channel})

  @doc false
  def delete(channel), do: GenServer.call(ChannelCache, {:delete, channel.id})

  def handle_call({:get, id}, _from, state) do
    ret =
      with nil <- Map.get(state, id),
           {:ok, guild} <- GuildCache.get_by(channel_id: id) do
        Enum.find(guild.channels, fn c -> c.id === id end)
      end

    {:reply, ret_to_struct(ret), state}
  end

  def handle_call({:create, id, channel}, _from, state) do
    {:reply, ret_to_struct(channel), Map.put(state, id, channel)}
  end

  def handle_call({:update, id, channel}, _from, state) do
    {old_channel, new_state} = Map.pop(state, id)
    {:reply, ret_to_struct({old_channel, channel}), Map.put(new_state, id, channel)}
  end

  def handle_call({:delete, id}, _from, state) do
    {old_channel, new_state} = Map.pop(state, id)
    {:reply, ret_to_struct(old_channel), new_state}
  end

  # Handle error case when id not found, from the get method
  def ret_to_struct({:error, _} = error), do: error
  # When fetching from a guild, the channel will already be a struct
  # TODO: Put into structs before storing
  def ret_to_struct(%{__struct__: _} = channel), do: {:ok, channel}

  def ret_to_struct({old, new}),
    do: {Util.cast(old, {:struct, Channel}), Util.cast(new, {:struct, Channel})}

  def ret_to_struct(channel), do: {:ok, Util.cast(channel, {:struct, Channel})}
end
