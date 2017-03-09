defmodule Nostrum.Cache.ChannelCache do
  @moduledoc """
  Cache for ChannelCache.
  """

  use GenServer

  alias Nostrum.Cache.Guild.GuildServer
  alias Nostrum.Struct.Channel
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

  Internally, the ChannelCache process only stores DMChannel references. To get
  channel information, a call is made to a `Nostrum.Cache.Guild.GuildServer`.
  """
  @spec get(id: integer | Nostrum.Struct.Message.t) :: {:error, atom} | channel
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

  def handle_call({:get, id}, _from, state) do
    ret =
      with \
        nil <- Map.get(state, id),
        {:ok, guild} <- GuildServer.get(channel_id: id)
      do
        Enum.find(guild.channels, fn channel ->
          channel.id == id
        end)
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

  # Handle error case when id not found
  def ret_to_struct({:error, _} = error), do: error
  # When fetching from a guild, the channel will already be a struct.
  def ret_to_struct(%{__struct__: _} = channel), do: channel
  def ret_to_struct({old, new}), do: {Channel.to_struct(old), Channel.to_struct(new)}
  def ret_to_struct(channel), do: Channel.to_struct(channel)

end
