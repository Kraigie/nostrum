defmodule Mixcord.Cache.Guild do
  @moduledoc """
  Cache for guilds.
  """

  use GenServer
  alias Mixcord.Cache.{Channel, User}
  alias Mixcord.Struct
  alias Mixcord.{Shard, Util}
  alias Supervisor.Spec
  require Logger

  @doc false
  def start_link(id, guild) do
    GenServer.start_link(__MODULE__, [id, guild])
  end

  @doc false
  def init([id, guild]) do
    case Registry.register(Guild.Registry, id, self()) do
      {:ok, _pid} ->
        {:ok, guild}
      {:error, {:already_started, _pid}} ->
        {:ok, guild}
      {:error, error} ->
        {:stop, error}
    end
  end

  @doc false
  def create(guild) do
    new_guild = create_guild_process!(guild.id, guild)
    if not Map.has_key?(new_guild, :unavailable) or not new_guild.unavailable do
      new_guild.members
      |> Enum.each(fn member -> User.create(member.user) end)
      new_guild.channels
      |> Enum.each(fn channel -> Channel.create(channel) end)
    end
    new_guild
  end

  @doc false
  def create(guild, shard_pid) do
    create(guild)
    Shard.request_guild_members(shard_pid, guild.id)
  end

  # TODO: Refactor this nasty code
  @doc false
  def create_guild_process!(id, guild) do
    case Supervisor.start_child(Guild.Supervisor, create_worker_spec(id, guild)) do
      {:error, {:already_started, pid}} ->
        handle_guild_unavailability!(pid, guild)
      {:error, reason} ->
        raise(Mixcord.Error.CacheError,
          "Could not start a new guild process with id #{id}, reason: #{inspect reason}")
      other ->
        Struct.Guild.to_struct(guild)
    end
  end

  def create_worker_spec(id, guild) do
    Spec.worker(__MODULE__, [id, guild], id: id)
  end

  @doc """
  When attempting to start a guild, if the guild is already created, check to see
  if it's unavailable, and if it is replace it with the new guild payload.
  """
  defp handle_guild_unavailability!(pid, guild) do
    if unavailable?(pid) do
      update_guild(pid, guild)
    else
      raise(Mixcord.Error.CacheError, "Attempted to create a child with an id that already exists")
    end
  end

  def unavailable?(pid) do
    GenServer.call(pid, {:unavailable})
  end

  def update_guild(pid, guild) do
    GenServer.call(pid, {:replace, guild})
  end

  def handle_call({:unavailable}, _from, state) do
    {:reply, state.unavailable, state}
  end

  def handle_call({:replace, guild}, _from, state) do
    {:reply, Struct.Guild.to_struct(guild), guild}
  end

  @spec get(id: integer) :: Mixcord.Struct.Guild.t
  @spec get(message: Mixcord.Struct.Message.t) :: Mixcord.Struct.Guild.t
  def get(id: id), do: :ok #GenServer.call(Guilds, {:get, :guild, id})
  def get(message: message), do: get(id: message.channel.guild_id)

  def get!(id: id) do
    get(id: id)
      |> Util.bangify_find(id, __MODULE__)
  end

  def get!(message: message) do
    get(message: message)
      |> Util.bangify_find(message, __MODULE__)
  end

  @doc false
  def update(guild), do: :ok #GenServer.call(Guilds, {:update, :guild, guild.id, guild})

  @doc false
  def delete(guild_id), do: :ok #GenServer.call(Guilds, {:delete, :guild, guild_id})

  @doc false
  def member_add(guild_id, member) do
     :ok #GenServer.call(Guilds, {:create, :member, guild_id, member})
     User.create(member.user)
  end

  @doc false
  def member_update(guild_id, user, _nick, roles), do: :ok #GenServer.call(Guilds, {:update, :member, guild_id, user, roles})

  @doc false
  def member_remove(guild_id, user), do: :ok #GenServer.call(Guilds, {:delete, :member, guild_id, user})

  @doc false
  def role_create(guild_id, role), do: :ok #GenServer.call(Guilds, {:create, :role, guild_id, role})

  @doc false
  def role_update(guild_id, role), do: :ok #GenServer.call(Guilds, {:update, :role, guild_id, role})

  @doc false
  def role_delete(guild_id, role_id), do: :ok #GenServer.call(Guilds, {:delete, :role, guild_id, role_id})

  @doc false
  def emoji_update(guild_id, emojis), do: :ok #GenServer.call(Guilds, {:update, :emoji, guild_id, emojis})

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
