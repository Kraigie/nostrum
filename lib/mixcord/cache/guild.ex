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
  def remove(guild_id) do
    GenServer.cast(Guilds, {:remove, guild_id})
  end

  @doc false
  def update(guild_id: guild_id, updated_guild: updated_guild) do
    GenServer.update(Guilds, {:update, guild_id, updated_guild: updated_guild})
  end

  def update(guild_id: guild_id, properties: properties) do
    GenServer.update(Guilds, {:update, guild_id, properties: properties})
  end

  def handle_cast({:create, guild}, state) do
    :ets.insert(:guilds, {guild.id, guild})
    {:noreply, state}
  end

  def handle_cast({:remove, id}, state) do
    :ets.delete(:guilds, id)
    {:noreply, state}
  end

  def handle_cast({:update, guild_id, updated_guild: updated_guild}), do: :ets.update_element(:guilds, guild_id, updated_guild)
  def handle_cast({:update, guild_id, properties: guild_props}) do
    old_guild = get!(id: guild_id)
    new_guild = Map.merge(old_guild, guild_props)
    :ets.insert(:guilds, {guild_id, guild_props})
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