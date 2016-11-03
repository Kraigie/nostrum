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
  # TODO: Move roles\members to seperate cache?

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
  def create(guild), do: GenServer.cast(Guilds, {:create, guild.id, guild: guild})

  @doc false
  def update(guild), do: GenServer.cast(Guilds, {:update, guild.id, guild: guild})

  @doc false
  def delete(guild_id), do: GenServer.cast(Guilds, {:delete, guild_id: guild_id})

  @doc false
  def emoji_update(guild_id, emojis), do: GenServer.cast(Guilds, {:update, guild_id, emojis: emojis})

  @doc false
  def member_add(guild_id, member), do: GenServer.cast(Guilds, {:create, guild_id, member: member})

  @doc false
  def member_update(guild_id, user, roles), do: GenServer.cast(Guilds, {:update, guild_id, user: user, roles: roles})

  @doc false
  def member_remove(guild_id, user), do: GenServer.cast(Guilds, {:delete, guild_id, member: user})

  @doc false
  def role_create(guild_id, role), do: GenServer.cast(Guilds, {:create, guild_id, role: role})

  @doc false
  def role_update(guild_id, role), do: GenServer.cast(Guilds, {:update, guild_id, role: role})

  @doc false
  def role_delete(guild_id, role_id), do: GenServer.cast(Guilds, {:delete, guild_id, role_id: role_id})

  def handle_cast({:create, guild_id, guild: guild}, state) do
    :ets.insert(:guilds, {guild_id, guild})
    {:noreply, state}
  end

  def handle_cast({:create, guild_id, member: member}, state) do
    guild = get!(id: guild_id)
    new_members = [member | guild.members]
    :ets.insert(:guilds, {guild_id, %{guild | members: new_members}})
    {:noreply, state}
  end

  def handle_cast({:create, guild_id, role: role}, state) do
    guild = get!(id: guild_id)
    new_roles = [role | guild.roles]
    :ets.insert(:guilds, {guild_id, %{guild | roles: new_roles}})
    {:noreply, state}
  end

  def handle_cast({:update, guild_id, guild: guild}, state) do
    :ets.update_element(:guilds, guild_id, guild)
    {:noreply, state}
  end

  def handle_cast({:update, guild_id, emojis: emojis}, state) do
    guild = get!(id: guild_id)
    :ets.insert(:guilds, {guild_id, %{guild | emojis: emojis}})
    {:noreply, state}
  end

  def handle_cast({:update, guild_id, user: user, roles: roles}, state) do
    guild = get!(id: guild_id)
    member_index = Enum.find_index(guild.members, fn member -> member.user.id == user.id end)
    members = List.update_at(guild.members, member_index,
      fn member ->
        %{member | user: user, roles: roles}
      end)
    :ets.insert(:guilds, {guild_id, %{guild | members: members}})
    {:noreply, state}
  end

  def handle_cast({:update, guild_id, role: role}, state) do
    guild = get!(id: guild_id)
    role_index = Enum.find_index(guild.roles, fn g_role -> g_role.id == role.id end)
    roles = List.update_at(guild.roles, role_index,
      fn g_role ->
        role
      end)
    :ets.insert(:guilds, {guild_id, %{guild | roles: roles}})
    {:noreply, state}
  end

  def handle_cast({:delete, guild: id}, state) do
    :ets.delete(:guilds, id)
    {:noreply, state}
  end

  def handle_cast({:delete, guild_id, member: user}, state) do
    guild = get!(id: guild_id)
    member_index = Enum.find_index(guild.members, fn member -> member.user.id == user.id end)
    members = List.delete_at(guild.members, member_index)
    :ets.insert(:guilds, {guild_id, %{guild | members: members}})
    {:noreply, state}
  end

  def handle_cast({:delete, guild_id, role_id: role_id}, state) do
    guild = get!(id: guild_id)
    role_index = Enum.find_index(guild.roles, fn g_role -> g_role.id == role_id end)
    roles = List.delete_at(guild.roles, role_index)
    :ets.insert(:guilds, {guild_id, %{guild | roles: roles}})
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