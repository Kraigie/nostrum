defmodule Nostrum.Cache.Guild.GuildServer do
  @moduledoc """
  Module for interacting with Guild Servers.
  """

  use GenServer

  alias Nostrum.Cache.Guild.GuildRegister
  alias Nostrum.Cache.Mapping.ChannelGuild
  alias Nostrum.Struct.Guild.{Channel, Member, Role}
  alias Nostrum.Struct.{Emoji, Guild}
  alias Nostrum.Util

  require Logger

  @typedoc """
  Represents different ways to search for a guild.

  ## Examples
    - Search by `t:Nostrum.Struct.Guild.id/0`
    ```Elixir
    GuildServer.get(id: 666666666666666)
    ```
    - Search by `t:Nostrum.Struct.Guild.TextChannel.id/0`
    ```Elixir
    GuildServer.get(channel_id: 314159265358979)
    ```
    - Search via `Nostrum.Struct.Message`
    ```Elixir
    msg = Nostrum.Api.create_message!(1618033988731, "Drake confirmed British")
    GuildServer.get(msg)
    ```
  """
  @type search_criteria :: [id: integer] |
                           [channel_id: integer] |
                           [message: Nostrum.Struct.Message.t]

  @typedoc """
  Represents different ways to get a value from a guild.

  Same as `t:search_criteria/0`, but takes an additional key parameter.
  Keys can be found in `Nostrum.Struct.Guild`
  """
  @type search_criteria_with_key :: [id: integer, key: key :: atom] |
                                    [channel_id: integer, key: key :: atom] |
                                    [message: Nostrum.Struct.Message.t, key: key :: atom]

  @typedoc """
  Transform for a guild.

  The function is passed a `Nostrum.Struct.Guild.t` struct.
  """
  @type transform :: (Guild.t -> term)

  @typedoc """
  Represents different ways to transform a guild.

  Same as `t:search_criteria/0`, but takes an additional transform parameter.
  See `transform/1` for an example.
  """
  @type search_criteria_with_transform :: [id: integer, transform: transform] |
                                          [channel_id: integer, transform: transform] |
                                          [message: Nostrum.Struct.Message.t, transform: transform]

  @doc """
  Retrieves a stream of all guilds.

  This will introduce a very large binary into the caller process. Consider
  running this method inside its own task.

  If you just need to get a specific key, consider using
  `get_value_all/1`

  ## Example
  ```Elixir
  Nostrum.Cache.Guild.GuildServer.all
  |> Enum.take_every(3)
  |> D3L3T3_GU1LD
  ```
  """
  @spec all() :: Enumerable.t
  def all do
    transform_all(&(&1))
  end

  @doc """
  Retrieves a stream of values from all guilds.

  ## Parameter
    - `key` - Value to get from all guilds.

  ## Example
  ```Elixir
  names =
  Nostrum.Cache.GuildServer.get_value_from_all(:name)
  |> Enum.filter(&is_weeb?)
  |> D3L3T3_GU1LD5
  ```
  """
  @spec get_value_from_all(key :: atom) :: Enumerable.t
  def get_value_from_all(key) when is_atom(key) do
    transform_all(&Map.get(&1, key))
  end

  @doc ~S"""
  Retrieves a guild from the cache.

  ## Parameters
    - `search_criteria` - The criteria used to search. See `t:search_criteria/0` for more info.

  ## Example
  ```Elixir
  case Nostrum.Cache.Guild.GuildServer.get(id: 1234567891234789) do
    {:ok, guild} -> "Guild has #{length(guild.members)} members"
    {:error, _reason} -> "Guild is MIA"
  end
  ```
  """
  @spec get(search_criteria) :: {:error, reason :: atom} | {:ok, Nostrum.Struct.Guild.t}
  def get(search_criteria) do
    transform(search_criteria ++ [transform: &(&1)])
  end

  @doc ~S"""
  Retrieves a guild from the cache.

  See `Nostrum.Cache.Guild.GuildServer.get/1` for usage.

  Raises `Nostrum.Error.CacheError` if unable to find the guild.
  """
  @spec get!(search_criteria) :: no_return | {Nostrum.Struct.Guild.t}
  def get!(search_criteria) do
    get(search_criteria)
    |> Util.bangify_find(search_criteria, __MODULE__)
  end

  @doc ~S"""
  Retrieves a value from a guild in the cache.

  This method should be preferred over the `get/1`
  method in the event that you need only one kv pair from the guild.

  ## Parameters
    - `search_criteria_with_key` - The criteria used to search. See
    `t:search_criteria_with_key/0` for more info.

  ## Example
  ```Elixir
  case Nostrum.Cache.Guild.Guildserver.get_value(id: 69696969696969, key: :name) do
    {:ok, sick_guild_name} -> "#{sick_guild_name} is sick"
    {:error, reason} -> "Whatchu' searchin' for fam?"
  end
  ```
  """
  @spec get_value(search_criteria_with_key) :: {:error, reason :: atom} |
                                               {:ok, term}
  def get_value(id: id, key: k) when is_atom(k) do
    transform(id: id, transform: &Map.get(&1, k)) |> check_missing_key
  end

  def get_value(channel_id: c_id, key: k) when is_atom(k) do
    transform(channel_id: c_id, transform: &Map.get(&1, k)) |> check_missing_key
  end

  def get_value(message: %Nostrum.Struct.Message{channel_id: c_id}, key: k) do
    c_id_int = String.to_integer(c_id)
    transform(channel_id: c_id_int, transform: &Map.get(&1, k)) |> check_missing_key
  end

  @doc """
  Retrieves a value from a guild in the cache.

  See `Nostrum.Cache.Guild.GuildServer.get_value!/2` for usage.

  Raises `Nostrum.Error.CacheError` if unable to find the guild or key.
  """
  @spec get_value!(search_criteria_with_key) :: no_return | term
  def get_value!(search_critera_with_key) do
    get_value(search_critera_with_key)
    |> bangify_key_search(search_critera_with_key[:key])
  end

  defp check_missing_key({:ok, nil}), do: {:error, :key_not_found}
  defp check_missing_key(ok), do: ok

  # REVIEW: Can/should we handle cache errors in a better way?
  defp bangify_key_search({:error, :key_not_found}, key),
    do: raise(Nostrum.Error.CacheError, key: key, cache_name: __MODULE__)
  defp bangify_key_search({:error, reason}, _key),
    do: raise(Nostrum.Error.CacheError, "ERROR: #{inspect reason}")
  defp bangify_key_search({:ok, val}, _key),
    do: val

  @doc """
  Retrieves a transformed guild from the cache.

  Returns the result of applying a given fun to the guild specified by the search
  criteria.

  This transform does not modify the cache and is performed entirely server side.
  This means that if you somehow cause an error to be thrown, the receiving guild
  process will die. The benefit is not "copying" guild binaries between
  processes which can be very big.

  ## Parameters
    - `search_criteria_with_transform` - The criteria used to search. See
    `t:search_criteria_with_transform/0` for more info.

  ## Example
  ```Elixir
  tf = fn guild -> Enum.any?(guild.members, &(Map.get(&1, :nick) == "The year is 20XX")) end
  {:ok, plays_fox?} = Nostrum.Cache.Guild.GuildServer.transform(id: 123, transform: tf)
  ```
  """
  @spec transform(search_criteria_with_transform) :: {:ok, term}
  def transform(id: id, transform: tf) do
    with {:ok, pid} <- GuildRegister.lookup(id),
    do: {:ok, GenServer.call(pid, {:transform, tf})}
  end

  def transform(channel_id: c_id, transform: tf) do
    with \
      {:ok, guild_id} <- ChannelGuild.get_guild(c_id),
      {:ok, guild_pid} <- GuildRegister.lookup(guild_id)
    do
      {:ok, GenServer.call(guild_pid, {:transform, tf})}
    end
  end

  def transform(message: %Nostrum.Struct.Message{channel_id: c_id}, transform: tf) do
    transform(channel_id: c_id, transform: tf)
  end

  @doc """
  Retrieves a stream of transformed guild values from the cache.

  Returns a stream of as the result of calling `transform` on every guild in the
  cache.

  See `transform/1` for usage.
  """
  @spec transform_all(transform) :: Enumerable.t
  def transform_all(transform) do
    Supervisor.which_children(GuildSupervisor)
    |> Stream.map(fn {_, pid, _, _} -> pid end)
    |> Task.async_stream(&GenServer.call(&1, {:transform, transform}))
    |> Stream.map(fn {:ok, term} -> term end)
  end

  @doc false
  # REVIEW: If a guild server crashes, it will be restarted with its initial state.
  def start_link(id, guild) do
    GenServer.start_link(__MODULE__, [id, guild])
  end

  @doc false
  def init([id, guild]) do
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
    with {:ok, pid} <- GuildRegister.lookup(id),
    do: GenServer.call(pid, request)
  end

  @doc false
  def create(guild) do
    # This returns {:ok, guild} or {:error reason}
    GuildRegister.create_guild_process(guild.id, guild)
  end

  @doc false
  def update(guild) do
    call(guild.id, {:update, guild})
  end

  @doc false
  def delete(guild_id) do
    call(guild_id, {:delete})
  end

  @doc false
  def member_add(guild_id, member) do
    call(guild_id, {:create, :member, member})
  end

  @doc false
  def member_update(guild_id, user, nick, roles) do
    call(guild_id, {:update, :member, user, nick, roles})
  end

  @doc false
  def member_remove(guild_id, user) do
    call(guild_id, {:delete, :member, user})
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
    call(guild_id, {:create, :role, role})
  end

  @doc false
  def role_update(guild_id, role) do
    call(guild_id, {:update, :role, role})
  end

  @doc false
  def role_delete(guild_id, role_id) do
    call(guild_id, {:delete, :role, role_id})
  end

  @doc false
  def emoji_update(guild_id, emojis) do
    call(guild_id, {:update, :emoji, emojis})
  end

  def handle_call({:transform, tf}, _from, state) do
    {:reply, tf.(state), state}
  end

  def handle_call({:update, guild}, _from, state) do
    {:reply, {Guild.to_struct(state), Guild.to_struct(guild)}, guild}
  end

  def handle_call({:delete}, _from, state) do
    {:stop, :normal, Guild.to_struct(state), %{}}
  end

  def handle_call({:create, :member, member}, _from, state) do
    new_members = [member | state.members]
    {:reply, Member.to_struct(member), %{state | members: new_members}}
  end

  def handle_call({:update, :member, user, nick, roles}, _from, state) do
    member_index = Enum.find_index(state.members, fn member ->
      member.user.id == user.id
    end)
    old_member =
      case Enum.fetch(state.members, member_index || length(state.members) + 1) do
        {:ok, old_member} -> old_member
        :error -> %{user: %{}, roles: []}
      end
    new_members = List.update_at(state.members, member_index, fn member ->
      %{member | user: user, nick: nick, roles: roles}
    end)

    {:reply, {Member.to_struct(old_member), Member.to_struct(%{old_member | user: user, nick: nick, roles: roles})}, %{state | members: new_members}}
  end

  def handle_call({:delete, :member, user}, _from, state) do
    member_index = Enum.find_index(state.members, fn member -> member.user.id == user.id end)
    deleted_member =
      case Enum.fetch(state.members, member_index || length(state.members) + 1) do
        {:ok, deleted_member} -> deleted_member
        :error -> %{}
      end
    members = List.delete_at(state.members, member_index)
    {:reply, Member.to_struct(deleted_member), %{state | members: members}}
  end

  def handle_call({:create, :channel, channel}, _from, state) do
    new_channels = [channel | state.channels]
    {:reply, Channel.to_struct(channel), %{state | channels: new_channels}}
  end

  def handle_call({:update, :channel, channel}, _from, state) do
    channel_index = Enum.find_index(state.channels, fn g_channel -> g_channel.id == channel.id end)
    old_channel =
      case Enum.fetch(state.channels, channel_index || length(state.channels) + 1) do
        {:ok, channel} -> channel
        :error -> %{}
      end
    channels = List.update_at(state.channels, channel_index,
      fn _ ->
        channel
      end)
    {:reply, {Channel.to_struct(old_channel), Channel.to_struct(channel)}, %{state | channels: channels}}
  end

  def handle_call({:delete, :channel, channel_id}, _from, state) do
    channel_index = Enum.find_index(state.channels, fn g_channel -> g_channel.id == channel_id end)
    old_channel =
      case Enum.fetch(state.channels, channel_index || length(state.channels) + 1) do
        {:ok, channel} -> channel
        :error -> %{}
      end
    channels = List.delete_at(state.channels, channel_index)
    {:reply, Channel.to_struct(old_channel), %{state | channels: channels}}
  end

  def handle_call({:create, :role, role}, _from, state) do
    new_roles = [role | state.roles]
    {:reply, Role.to_struct(role), %{state | roles: new_roles}}
  end

  def handle_call({:update, :role, role}, _from, state) do
    role_index = Enum.find_index(state.roles, fn g_role -> g_role.id == role.id end)
    old_role =
      case Enum.fetch(state.roles, role_index || length(state.roles) + 1) do
        {:ok, role} -> role
        :error -> %{}
      end
    roles = List.update_at(state.roles, role_index,
      fn _ ->
        role
      end)
    {:reply, {Role.to_struct(old_role), Role.to_struct(role)}, %{state | roles: roles}}
  end

  def handle_call({:delete, :role, role_id}, _from, state) do
    role_index = Enum.find_index(state.roles, fn g_role -> g_role.id == role_id end)
    old_role =
      case Enum.fetch(state.roles, role_index || length(state.roles) + 1) do
        {:ok, role} -> role
        :error -> %{}
      end
    roles = List.delete_at(state.roles, role_index)
    {:reply, Role.to_struct(old_role), %{state | roles: roles}}
  end

  def handle_call({:update, :emoji, emojis}, _from, state) do
    old_emojis = state.emojis
    {:reply, {Emoji.to_struct(old_emojis), Emoji.to_struct(emojis)}, %{state | emojis: emojis}}
  end

end
