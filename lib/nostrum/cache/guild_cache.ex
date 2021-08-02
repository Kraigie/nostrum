defmodule Nostrum.Cache.GuildCache do
  @table_name :nostrum_guilds
  @moduledoc """
  Functions for retrieving guild states.

  The ETS table name associated with the Guild Cache is `#{@table_name}`.
  Besides the methods provided here, you can call any other ETS methods
  on the table.
  """

  alias Nostrum.Cache.Mapping.ChannelGuild
  alias Nostrum.Snowflake
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Message
  alias Nostrum.Util

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @type clause ::
          {:id, Guild.id()}
          | {:channel_id, Channel.id()}
          | {:message, Message.t()}

  @type clauses :: [clause] | map

  @type selector :: (Guild.t() -> any)

  @type reason ::
          :id_not_found
          | :id_not_found_on_guild_lookup

  defguardp is_selector(term) when is_function(term, 1)

  @doc "Retrieve the ETS table name used for the cache."
  @spec tabname :: atom()
  def tabname, do: @table_name

  @doc """
  Retrieves all `Nostrum.Struct.Guild` from the cache as a list.
  """
  @spec all() :: Enum.t()
  def all do
    @table_name
    |> :ets.tab2list()
    |> Stream.map(&elem(&1, 1))
  end

  @doc """
  Selects values using a `selector` from all `Nostrum.Struct.Guild` in the cache.
  """
  @spec select_all(selector) :: Enum.t()
  def select_all(selector)

  def select_all(selector) when is_selector(selector) do
    :ets.foldl(fn {_id, guild}, acc -> [selector.(guild) | acc] end, [], @table_name)
  end

  @doc """
  Retrives a single `Nostrum.Struct.Guild` from the cache via its `id`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```Elixir
  iex> Nostrum.Cache.GuildCache.get(0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get(10)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec get(Guild.id()) :: {:ok, Guild.t()} | {:error, reason}
  def get(id) do
    select(id, fn guild -> guild end)
  end

  @doc ~S"""
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get!(Guild.id()) :: Guild.t() | no_return
  def get!(id), do: get(id) |> Util.bangify_find(id, __MODULE__)

  @doc """
  Retrives a single `Nostrum.Struct.Guild` where it matches the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```Elixir
  iex> Nostrum.Cache.GuildCache.get_by(id: 0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(%{id: 0})
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(id: 10)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec get_by(clauses) :: {:ok, Guild.t()} | {:error, reason}
  def get_by(clauses) do
    select_by(clauses, fn guild -> guild end)
  end

  @doc ~S"""
  Same as `get_by/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get_by!(clauses) :: Guild.t() | no_return
  def get_by!(clauses), do: get_by(clauses) |> Util.bangify_find(clauses, __MODULE__)

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```Elixir
  iex> Nostrum.Cache.GuildCache.select(0, fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select(10, fn guild -> guild.id end)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec select(Guild.id(), selector) :: {:ok, any} | {:error, reason}
  def select(id, selector) do
    select_by(%{id: id}, selector)
  end

  @doc ~S"""
  Same as `select/2`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec select!(Guild.id(), selector) :: any | no_return
  def select!(id, selector), do: select(id, selector) |> Util.bangify_find(id, __MODULE__)

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild` that matches
  the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```Elixir
  iex> Nostrum.Cache.GuildCache.select_by([id: 0], fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select_by(%{id: 0}, fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select_by([id: 10], fn guild -> guild.id end)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec select_by(clauses, selector) :: {:ok, any} | {:error, reason}
  def select_by(clauses, selector)

  def select_by(clauses, selector) when is_list(clauses) and is_selector(selector),
    do: select_by(Map.new(clauses), selector)

  def select_by(%{id: id}, selector) when is_snowflake(id) and is_selector(selector) do
    case :ets.lookup(@table_name, id) do
      [{^id, guild}] ->
        selection = selector.(guild)
        {:ok, selection}

      [] ->
        {:error, :id_not_found_on_guild_lookup}
    end
  end

  def select_by(%{channel_id: channel_id}, selector)
      when is_snowflake(channel_id) and is_selector(selector) do
    case ChannelGuild.get_guild(channel_id) do
      {:ok, guild_id} -> select_by(%{id: guild_id}, selector)
      {:error, _} = error -> error
    end
  end

  def select_by(%{message: %Message{channel_id: channel_id}}, selector) do
    select_by(%{channel_id: channel_id}, selector)
  end

  @doc ~S"""
  Same as `select_by/2`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec select_by!(clauses, selector) :: any | no_return
  def select_by!(clauses, selector),
    do: select_by(clauses, selector) |> Util.bangify_find(clauses, __MODULE__)

  # IMPLEMENTATION
  @doc false
  @spec create(Guild.t()) :: true
  def create(guild) do
    true = :ets.insert_new(@table_name, {guild.id, guild})
  end

  @doc false
  @spec update(map()) :: true
  def update(payload) do
    [{_id, guild}] = :ets.lookup(@table_name, payload.id)
    new_guild = Map.merge(guild, payload)
    true = :ets.update_element(@table_name, payload.id, {2, new_guild})
  end

  @doc false
  @spec delete(Guild.id()) :: Guild.t() | nil
  def delete(guild_id) do
    # Returns the old guild, if cached
    case :ets.take(@table_name, guild_id) do
      [guild] -> guild
      [] -> nil
    end
  end

  @doc false
  @spec channel_create(Guild.id(), map()) :: Channel.t()
  def channel_create(guild_id, channel) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    new_channel = Util.cast(channel, {:struct, Channel})
    new_channels = Map.put(guild.channels, channel.id, new_channel)
    new_guild = %{guild | channels: new_channels}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    new_channel
  end

  @doc false
  @spec channel_delete(Guild.id(), Channel.id()) :: Channel.t() | :noop
  def channel_delete(guild_id, channel_id) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {popped, new_channels} = Map.pop(guild.channels, channel_id)
    new_guild = %{guild | channels: new_channels}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    if popped, do: popped, else: :noop
  end

  @doc false
  @spec channel_update(Guild.id(), map()) :: {Channel.t(), Channel.t()}
  def channel_update(guild_id, channel) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {old, new, new_channels} = upsert(guild.channels, channel.id, channel, Channel)
    new_guild = %{guild | channels: new_channels}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {old, new}
  end

  @doc false
  @spec emoji_update(Guild.id(), [Emoji.t()]) :: {[Emoji.t()], [Emoji.t()]}
  def emoji_update(guild_id, emojis) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    new = %{guild | emojis: emojis}
    true = :ets.update_element(@table_name, guild_id, {2, new})
    {guild.emojis, emojis}
  end

  @doc false
  @spec member_add(Guild.id(), map()) :: Member.t()
  def member_add(guild_id, payload) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {_old, member, new_members} = upsert(guild.members, payload["user"]["id"], payload, Member)
    new = %{guild | members: new_members, member_count: guild.member_count + 1}
    true = :ets.update_element(@table_name, guild_id, {2, new})
    member
  end

  @doc false
  @spec member_remove(Guild.id(), map()) :: {Guild.id(), Member.t()} | :noop
  def member_remove(guild_id, user) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {popped, new_members} = Map.pop(guild.members, user["id"])
    new_guild = %{guild | members: new_members, member_count: guild.member_count - 1}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    if popped, do: {guild_id, popped}, else: :noop
  end

  @doc false
  @spec member_update(Guild.id(), map()) :: {Member.t(), Member.t()}
  def member_update(guild_id, member) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {old, new, new_members} = upsert(guild.members, member["user"]["id"], member, Member)
    new_guild = %{guild | members: new_members}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {old, new}
  end

  @doc false
  def member_chunk(guild_id, member_chunk) do
    # CHONK like that one cat of craig

    [{_id, guild}] = :ets.lookup(@table_name, guild_id)

    new_members =
      Enum.reduce(member_chunk, guild.members, fn m, acc ->
        Map.put(acc, m.user.id, Util.cast(m, {:struct, Member}))
      end)

    # XXX: do we not need to update member count here?
    new = %{guild | members: new_members}
    true = :ets.update_element(@table_name, guild_id, {2, new})
  end

  @doc false
  @spec role_create(Guild.id(), map()) :: Role.t()
  def role_create(guild_id, role) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {_old, new, new_roles} = upsert(guild.roles, role.id, role, Role)
    new_guild = %{guild | roles: new_roles}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    new
  end

  @doc false
  @spec role_delete(Guild.id(), Role.id()) :: {Guild.id(), Role.t()} | :noop
  def role_delete(guild_id, role_id) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {popped, new_roles} = Map.pop(guild.roles, role_id)
    new_guild = %{guild | roles: new_roles}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    if popped, do: {guild_id, popped}, else: :noop
  end

  @doc false
  @spec role_update(Guild.id(), map()) :: {Role.t(), Role.t()}
  def role_update(guild_id, role) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {old, new_role, new_roles} = upsert(guild.roles, role.id, role, Role)
    new_guild = %{guild | roles: new_roles}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {old, new_role}
  end

  @doc false
  @spec voice_state_update(Guild.id(), map()) :: {Guild.id(), [map()]}
  def voice_state_update(guild_id, payload) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    lighter_payload = Map.delete(payload, :member)
    state_without_user = Enum.reject(guild.voice_states, &(&1.user_id == lighter_payload.user_id))
    # If the `channel_id` is nil, then the user is leaving.
    # Otherwise, the voice state was updated.
    new_state =
      if(is_nil(lighter_payload.channel_id),
        do: state_without_user,
        else: [lighter_payload | state_without_user]
      )

    new_guild = %{guild | voice_states: new_state}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {guild_id, new_state}
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
