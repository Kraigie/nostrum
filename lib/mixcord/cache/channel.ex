defmodule Mixcord.Cache.Channel do
  @moduledoc """
  Cache for channels.

  The ETS table name associated with the Channel Cache is `:channels`. Besides the
  methods provided below you can call any other ETS methods on the table.

  ## Example
  ```elixir
  info = :ets.info(:channels)
  [..., heir: :none, name: :channels, size: 1, ...]
  size = info[:size]
  1
  ```
  """

  use GenServer
  alias Mixcord.Map.{TextChannel, VoiceChannel}
  alias Mixcord.Util

  @type channel :: Mixcord.Map.TextChannel.t | Mixcord.Map.VoiceChannel.t

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, [], name: Channels)
  end

  def init(_args) do
    :ets.new(:channels, [:set, :public, :named_table])
    {:ok, []}
  end

  @spec get(id: integer) :: channel
  @spec get(message: Mixcord.Map.Message.t) :: channel
  def get(id: id), do: :ets.lookup_element(:channels, id, 2)
  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find
  end


  @doc false
  def create(channel), do: GenServer.cast(Channels, {:create, channel.id, channel: channel})

  @doc false
  def update(channel), do: GenServer.cast(Channels, {:update, channel.id, channel: channel})

  @doc false
  def delete(channel), do: GenServer.cast(Channels, {:delete, channel_id: channel.id})

  def handle_cast({:create, channel_id, channel: channel}, state) do
    :ets.insert(:channels, {channel_id, channel})
    {:noreply, state}
  end

  def handle_cast({:update, channel_id, channel: channel}, state) do
    :ets.update_element(:channels, channel_id, channel)
    {:noreply, state}
  end

  def handle_cast({:delete, channel_id: id}, state) do
    :ets.delete(:channels, id)
    {:noreply, state}
  end

end