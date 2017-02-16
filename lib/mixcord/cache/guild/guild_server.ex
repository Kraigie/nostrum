defmodule Mixcord.Cache.Guild.GuildServer do
  @moduledoc """
  Cache for guilds.
  """

  use GenServer
  alias Mixcord.Cache.{ChannelCache, UserCache}
  alias Mixcord.Cache.Guild.GuildRegister
  alias Mixcord.Struct.{Guild}
  alias Mixcord.{Shard, Util}
  alias Supervisor.Spec
  require Logger

  @doc false
  def start_link(id, guild) do
    GenServer.start_link(__MODULE__, [id, guild])
  end

  @doc false
  def init([id, guild]) do
    case Registry.register(GuildRegistry, id, self()) do
      {:ok, _pid} ->
        {:ok, guild}
      {:error, {:already_started, _pid}} ->
        {:ok, guild}
      {:error, error} ->
        {:stop, error}
    end
  end

  @spec get(id: integer) :: Mixcord.Struct.Guild.t
  @spec get(message: Mixcord.Struct.Message.t) :: Mixcord.Struct.Guild.t
  def get(id: id) do
    case GuildRegister.lookup(id) do
      {:ok, pid} ->
        GenServer.call(pid, {:get, :guild, id})
      error ->
        error
    end
  end

  def get(message: message) do
    case GuildRegister.lookup(message.channel.guild_id) do
      {:ok, pid} ->
        GenServer.call(pid, {:get, :guild, message.channel.guild_id})
      error ->
        error
    end
  end

  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find(id, __MODULE__)
  end

  def get!(message: message) do
    get(message: message)
      |> Util.bangify_find(message, __MODULE__)
  end

  @doc false
  def unavailable?(pid) do
    GenServer.call(pid, {:unavailable})
  end

  @doc false
  def make_guild_available(pid, guild) do
    GenServer.call(pid, {:replace, guild})
  end

  @doc false
  def create(guild) do
    new_guild = GuildRegister.create_guild_process!(guild.id, guild)
    if not Map.has_key?(new_guild, :unavailable) or not new_guild.unavailable do
      new_guild.members
      |> Enum.each(fn member -> UserCache.create(member.user) end)
      new_guild.channels
      |> Enum.each(fn channel -> ChannelCache.create(channel) end)
    end
    new_guild
  end

  @doc false
  def create(guild, shard_pid) do
    create(guild)
    Shard.request_guild_members(shard_pid, guild.id)
  end

  @doc false
  def update(guild) do
    pid = GuildRegister.lookup!(guild.id)
    GenServer.call(pid, {:update, :guild, guild.id, guild})
  end

  @doc false
  def delete(guild_id) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:delete, :guild, guild_id})
  end

  @doc false
  def member_add(guild_id, member) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:create, :member, guild_id, member})
    UserCache.create(member.user)
  end

  @doc false
  def member_update(guild_id, user, _nick, roles) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:update, :member, guild_id, user, roles})
  end

  @doc false
  def member_remove(guild_id, user) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:delete, :member, guild_id, user})
  end

  @doc false
  def role_create(guild_id, role) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:create, :role, guild_id, role})
  end

  @doc false
  def role_update(guild_id, role) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:update, :role, guild_id, role})
  end

  @doc false
  def role_delete(guild_id, role_id) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:delete, :role, guild_id, role_id})
  end

  @doc false
  def emoji_update(guild_id, emojis) do
    pid = GuildRegister.lookup!(guild_id)
    GenServer.call(pid, {:update, :emoji, guild_id, emojis})
  end

  def handle_call({:replace, guild}, _from, state) do
    {:reply, Guild.to_struct(guild), guild}
  end

  def handle_call({:unavailable}, _from, state) do
    {:reply, state.unavailable, state}
  end

  def handle_call({:get, :guild, id}, _from, state) do
    {:reply, Map.get(state, id), state}
  end

  def handle_call({:create, :guild, id, guild}, _from, state) do
    {:reply, guild, Map.put(state, id, guild)}
  end

  def handle_call({:update, :guild, id, guild}, _from, state) do
    {old_guild, new_state} = Map.pop(state, id)
    {:reply, {old_guild, guild}, Map.put(new_state, id, guild)}
  end

  def handle_call({:delete, :guild, id}, _from, state) do
    {old_guild, new_state} = Map.pop(state, id)
    {:reply, old_guild, new_state}
  end

  def handle_call({:create, :member, guild_id, member}, _from, state) do
    guild = Map.get(state, guild_id)
    new_members = [member | guild.members]
    {:reply, member, Map.put(state, guild_id, %{guild | members: new_members})}
  end

  def handle_call({:update, :member, guild_id, user, roles}, _from, state) do
    guild = Map.get(state, guild_id)
    member_index = Enum.find_index(guild.members, fn member ->
      member.user.id == user.id
    end)
    old_member =
      case Enum.fetch(state, member_index) do
        {:ok, old_member} -> old_member
        :error -> %{user: %{}, roles: []}
      end
    new_members = List.update_at(guild.members, member_index, fn member ->
      %{member | user: user, roles: roles}
    end)
    new_state = Map.update(state, guild_id, [], fn guild ->
      %{guild | members: new_members}
    end)
    {:reply, {old_member, %{old_member | user: user, roles: roles}}, new_state}
  end

  def handle_call({:delete, :member, guild_id, user}, _from, state) do
    guild = Map.get(state, guild_id)
    member_index = Enum.find_index(guild.members, fn member -> member.user.id == user.id end)
    deleted_member =
      case Enum.fetch(state, member_index) do
        {:ok, deleted_member} -> deleted_member
        :error -> %{}
      end
    members = List.delete_at(guild.members, member_index)
    new_state = Map.update(state, guild_id, [], fn guild ->
      %{guild | members: members}
    end)
    {:reply, deleted_member, new_state}
  end

  def handle_call({:create, :role, guild_id, role}, _from, state) do
    guild = Map.get(state, guild_id)
    new_roles = [role | guild.roles]
    {:reply, role, Map.put(state, guild_id, %{guild | roles: new_roles})}
  end

  def handle_call({:update, :role, guild_id, role}, _from, state) do
    guild = Map.get(state, guild_id)
    role_index = Enum.find_index(guild.roles, fn g_role -> g_role.id == role.id end)
    old_role =
      case Enum.fetch(state, role_index) do
        {:ok, role} -> role
        :error -> %{}
      end
    roles = List.update_at(guild.roles, role_index,
      fn _ ->
        role
      end)
      new_state = Map.update(state, guild_id, [], fn guild ->
        %{guild | roles: roles}
      end)
    {:reply, {old_role, role}, new_state}
  end

  def handle_call({:delete, :role, guild_id, role_id}, _from, state) do
    guild = Map.get(state, guild_id)
    role_index = Enum.find_index(guild.roles, fn g_role -> g_role.id == role_id end)
    old_role =
      case Enum.fetch(state, role_index) do
        {:ok, role} -> role
        :error -> %{}
      end
    roles = List.delete_at(guild.roles, role_index)
    new_state = Map.update(state, guild_id, [], fn guild ->
      %{guild | roles: roles}
    end)
    {:reply, old_role, new_state}
  end

  def handle_call({:update, :emoji, guild_id, emojis}, _from, state) do
    guild = Map.get(state, guild_id)
    old_emojis = guild.emojis
    new_state = Map.update(state, guild_id, [], fn guild ->
      %{guild | emojis: emojis}
    end)
    {:reply, {old_emojis, emojis}, new_state}
  end

end
