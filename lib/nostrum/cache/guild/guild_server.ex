defmodule Nostrum.Cache.Guild.GuildServer do
  @moduledoc false

  use GenServer, restart: :transient

  alias Nostrum.Cache.Guild.GuildRegister
  alias Nostrum.Struct.{Channel, Guild}
  alias Nostrum.Struct.Guild.{Member, Role}
  alias Nostrum.Util

  require Logger

  @doc false
  # REVIEW: If a guild server crashes, it will be restarted with its initial state.
  def start_link(init) do
    GenServer.start_link(__MODULE__, init)
  end

  @doc false
  def init(id: id, guild: guild) do
    case Registry.register(GuildRegistry, id, self()) do
      {:ok, _pid} ->
        {:ok, guild}

      {:error, error} ->
        # Causes start_link to return {:error, reason}
        {:stop, error}
    end
  end

  @doc false
  def call(id, request) do
    with {:ok, pid} <- GuildRegister.lookup(id), do: GenServer.call(pid, request)
  end

  def cast(id, request) do
    with {:ok, pid} <- GuildRegister.lookup(id), do: GenServer.cast(pid, request)
  end

  @doc false
  @spec create(Guild.t()) :: {:ok, Guild.t()} | {:error, term}
  def create(guild) do
    GuildRegister.create_guild_process(guild.id, guild)
  end

  @doc false
  def update(guild) do
    call(guild.id, {:update, guild})
  end

  # Uses a selector function to select certain fields from the guild state.
  @doc false
  @spec select(Guild.id(), (Guild.t() -> any)) :: any
  def select(guild_id, fun) do
    call(guild_id, {:select, fun})
  end

  @doc false
  def delete(guild_id) do
    call(guild_id, {:delete})
  end

  @doc false
  def member_add(guild_id, member) do
    call(guild_id, {:create, :member, guild_id, member})
  end

  @doc false
  def member_add_from_presence(guild_id, member) do
    cast(guild_id, {:create, :member, member})
  end

  @doc false
  def member_update(guild_id, member) do
    call(guild_id, {:update, :member, guild_id, member})
  end

  @doc false
  def member_remove(guild_id, user) do
    call(guild_id, {:delete, :member, guild_id, user})
  end

  def member_chunk(guild_id, user) do
    cast(guild_id, {:chunk, :member, user})
  end

  @doc false
  def channel_create(guild_id, channel) do
    call(guild_id, {:create, :channel, channel})
  end

  @doc false
  def channel_update(guild_id, channel) do
    call(guild_id, {:update, :channel, channel})
  end

  @doc false
  def channel_delete(guild_id, channel_id) do
    call(guild_id, {:delete, :channel, channel_id})
  end

  @doc false
  def role_create(guild_id, role) do
    call(guild_id, {:create, :role, guild_id, role})
  end

  @doc false
  def role_update(guild_id, role) do
    call(guild_id, {:update, :role, guild_id, role})
  end

  @doc false
  def role_delete(guild_id, role_id) do
    call(guild_id, {:delete, :role, guild_id, role_id})
  end

  @doc false
  def emoji_update(guild_id, emojis) do
    call(guild_id, {:update, :emoji, guild_id, emojis})
  end

  def handle_call({:select, fun}, _from, state) do
    {:reply, fun.(state), state, :hibernate}
  end

  def handle_call({:update, guild}, _from, state) do
    new_guild =
      state
      |> Map.from_struct()
      |> Map.merge(guild)
      |> Util.cast({:struct, Guild})

    {:reply, {state, new_guild}, new_guild, :hibernate}
  end

  def handle_call({:delete}, _from, state) do
    {:stop, :normal, state, %{}}
  end

  def handle_call(
        {:create, :member, guild_id, %{user: %{id: id}} = member},
        _from,
        %{members: members} = state
      ) do
    {_old, new, new_map} = upsert(members, id, member, Member)

    {:reply, {guild_id, new}, %{state | members: new_map, member_count: state.member_count + 1},
     :hibernate}
  end

  def handle_call(
        {:update, :member, guild_id, %{user: %{id: id}} = partial_member},
        _from,
        %{members: members} = state
      ) do
    {old, new, new_map} = upsert(members, id, partial_member, Member)
    {:reply, {guild_id, old, new}, %{state | members: new_map}, :hibernate}
  end

  def handle_call({:delete, :member, guild_id, %{id: id}}, _from, %{members: members} = state) do
    {popped, new} = Map.pop(members, id)
    ret = if popped, do: {guild_id, popped}, else: :noop
    {:reply, ret, %{state | members: new, member_count: state.member_count - 1}, :hibernate}
  end

  def handle_call({:create, :channel, %{id: id} = channel}, _from, %{channels: channels} = state) do
    {_old, new, new_map} = upsert(channels, id, channel, Channel)
    {:reply, new, %{state | channels: new_map}, :hibernate}
  end

  def handle_call({:update, :channel, %{id: id} = channel}, _from, %{channels: channels} = state) do
    {old, new, new_map} = upsert(channels, id, channel, Channel)
    {:reply, {old, new}, %{state | channels: new_map}, :hibernate}
  end

  def handle_call({:delete, :channel, channel_id}, _from, %{channels: channels} = state) do
    {popped, new} = Map.pop(channels, channel_id)
    ret = if popped, do: popped, else: :noop
    {:reply, ret, %{state | channels: new}, :hibernate}
  end

  def handle_call({:create, :role, guild_id, %{id: id} = role}, _from, %{roles: roles} = state) do
    {_old, new, new_map} = upsert(roles, id, role, Role)
    {:reply, {guild_id, new}, %{state | roles: new_map}, :hibernate}
  end

  def handle_call({:update, :role, guild_id, %{id: id} = role}, _from, %{roles: roles} = state) do
    {old, new, new_map} = upsert(roles, id, role, Role)
    {:reply, {guild_id, old, new}, %{state | roles: new_map}, :hibernate}
  end

  def handle_call({:delete, :role, guild_id, role_id}, _from, %{roles: roles} = state) do
    {popped, new} = Map.pop(roles, role_id)
    ret = if popped, do: {guild_id, popped}, else: :noop
    {:reply, ret, %{state | roles: new}, :hibernate}
  end

  def handle_call({:update, :emoji, guild_id, emojis}, _from, state) do
    old_emojis = state.emojis
    {:reply, {guild_id, old_emojis, emojis}, %{state | emojis: emojis}, :hibernate}
  end

  def handle_cast({:chunk, :member, new_members}, %{members: members} = state) do
    new_members =
      Enum.reduce(new_members, members, fn m, acc ->
        Map.put(acc, m.user.id, Util.cast(m, {:struct, Member}))
      end)

    {:noreply, %{state | members: new_members}, :hibernate}
  end

  @spec upsert(%{required(Snowflake.t()) => struct}, Snowflake.t(), map, atom) ::
          {struct | nil, struct, %{required(Snowflake.t()) => struct}}
  defp upsert(map, key, new, struct) do
    if Map.has_key?(map, key) do
      old = Map.get(map, key)

      new =
        old
        |> Map.from_struct()
        |> Map.merge(new)
        |> Util.cast({:struct, struct})

      new_map = Map.put(map, key, new)

      {old, new, new_map}
    else
      new = Util.cast(new, {:struct, struct})
      {nil, new, Map.put(map, key, new)}
    end
  end
end
