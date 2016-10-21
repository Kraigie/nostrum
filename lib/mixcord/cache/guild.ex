defmodule Mixcord.Cache.Guild do
  @moduledoc """
  Cache for guilds.

  The ETS table name associated with the Guild Cache is `:guilds`. Besides the
  methods provided below you can call any other ETS methods on the table.

  ## Example
  ```elixir
  iex> info = :ets.info(:guilds)
  [..., heir: :none, name: :guilds, size: 1, ...]
  iex> size = info[:size]
  1
  ```
  """

  use GenServer

  # TODO: Length\Update?

  def start_link do
    GenServer.start_link(__MODULE__, [], name: Guilds)
  end

  def init(_args) do
    :ets.new(:guilds, [:set, :public, :named_table])
    {:ok, []}
  end

  def get(id: id), do: :ets.lookup_element(:guilds, id, 2)
  def get(message: message), do: get(id: message.channel.guild_id)

  def get!(id: id) do
    get(id: id)
      |> bangify_find
  end

  def get!(message: message) do
    get(message: message)
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

  defp handle_cast({:create, guild}, state) do
    :ets.insert(:guilds, {guild.id, guild})
    {:noreply, state}
  end

  defp handle_cast({:remove, id}, state) do
    :ets.delete(:guilds, {id})
    {:noreply, state}
  end

  defp bangify_find(to_bang) do
    case to_bang do
      nil ->
        raise(Mixcord.Error.CacheError)
      ret ->
        ret
    end
  end

end