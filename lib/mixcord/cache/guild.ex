defmodule Mixcord.Cache.Guild do
  @moduledoc """
  Cache for guilds.
  """

  use GenServer
  alias Mixcord.Cache.{Channel, User}
  alias Mixcord.Shard
  alias Mixcord.Util

  @doc false
  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: Guilds)
  end

  def init(state) do
    {:ok, state}
  end

  @spec get(id: integer) :: Mixcord.Map.Guild.t
  @spec get(message: Mixcord.Map.Message.t) :: Mixcord.Map.Guild.t
  def get(id: id), do: GenServer.call(Guilds, {:get, :guild, id})
  def get(message: message), do: get(id: message.channel.guild_id)

  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find
  end

  def get!(message: message) do
    get(message: message)
      |> Util.bangify_find
  end

  @doc false
  def create(guild) do
    new_guild = GenServer.call(Guilds, {:create, :guild, guild.id, guild})
    if not guild.unavailable do
      guild.members
        |> Enum.each(fn member -> User.create(member.user) end)
      guild.channels
        |> Enum.each(fn channel -> Channel.create(channel) end)
    end
    new_guild
  end

  def create(guild, shard_pid) do
    create(guild)
    Shard.request_guild_members(shard_pid, guild.id)
  end

  @doc false
  def update(guild), do: GenServer.call(Guilds, {:update, :guild, guild.id, guild})

  @doc false
  def delete(guild_id), do: GenServer.call(Guilds, {:delete, :guild, guild_id})

  @doc false
  def member_add(guild_id, member) do
     GenServer.call(Guilds, {:create, :member, guild_id, member})
     User.create(member.user)
  end

  @doc false
  def member_update(guild_id, user, _nick, roles), do: GenServer.call(Guilds, {:update, :member, guild_id, user, roles})

  @doc false
  def member_remove(guild_id, user), do: GenServer.call(Guilds, {:delete, :member, guild_id, user})

  @doc false
  def role_create(guild_id, role), do: GenServer.call(Guilds, {:create, :role, guild_id, role})

  @doc false
  def role_update(guild_id, role), do: GenServer.call(Guilds, {:update, :role, guild_id, role})

  @doc false
  def role_delete(guild_id, role_id), do: GenServer.call(Guilds, {:delete, :role, guild_id, role_id})

  @doc false
  def emoji_update(guild_id, emojis), do: GenServer.call(Guilds, {:update, :emoji, guild_id, emojis})

  def handle_call({:get, :guild, id}, _from, state) do

  end

  def handle_call({:create, :guild, id, guild}, _from, state) do
    {:reply, %{}, state}
  end

  def handle_call({:update, :guild, guild_id, guild}, state) do
    :ets.update_element(:guilds, guild_id, guild)
    {:noreply, state}
  end

  def handle_call({:delete, :guild, id}, state) do
    :ets.delete(:guilds, id)
    {:noreply, state}
  end

  def handle_call({:create, :member, guild_id, member}, state) do
    guild = get!(id: guild_id)
    new_members = [member | guild.members]
    :ets.insert(:guilds, {guild_id, %{guild | members: new_members}})
    {:noreply, state}
  end

  def handle_call({:update, :member, guild_id, user, roles}, state) do
    guild = get!(id: guild_id)
    member_index = Enum.find_index(guild.members, fn member -> member.user.id == user.id end)
    members = List.update_at(guild.members, member_index,
      fn member ->
        %{member | user: user, roles: roles}
      end)
    :ets.insert(:guilds, {guild_id, %{guild | members: members}})
    {:noreply, state}
  end

  def handle_call({:delete, :member, guild_id, user}, state) do
    guild = get!(id: guild_id)
    member_index = Enum.find_index(guild.members, fn member -> member.user.id == user.id end)
    members = List.delete_at(guild.members, member_index)
    :ets.insert(:guilds, {guild_id, %{guild | members: members}})
    {:noreply, state}
  end

  def handle_call({:create, :role, guild_id, role}, state) do
    guild = get!(id: guild_id)
    new_roles = [role | guild.roles]
    :ets.insert(:guilds, {guild_id, %{guild | roles: new_roles}})
    {:noreply, state}
  end

  def handle_call({:update, :role, guild_id, role}, state) do
    guild = get!(id: guild_id)
    role_index = Enum.find_index(guild.roles, fn g_role -> g_role.id == role.id end)
    roles = List.update_at(guild.roles, role_index,
      fn _ ->
        role
      end)
    :ets.insert(:guilds, {guild_id, %{guild | roles: roles}})
    {:noreply, state}
  end

  def handle_call({:delete, :role, guild_id, role_id}, state) do
    guild = get!(id: guild_id)
    role_index = Enum.find_index(guild.roles, fn g_role -> g_role.id == role_id end)
    roles = List.delete_at(guild.roles, role_index)
    :ets.insert(:guilds, {guild_id, %{guild | roles: roles}})
    {:noreply, state}
  end

  def handle_call({:update, :emoji, guild_id, emojis}, state) do
    guild = get!(id: guild_id)
    :ets.insert(:guilds, {guild_id, %{guild | emojis: emojis}})
    {:noreply, state}
  end

end