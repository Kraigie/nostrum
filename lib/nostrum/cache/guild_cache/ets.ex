defmodule Nostrum.Cache.GuildCache.ETS do
  @table_name :nostrum_guilds
  @moduledoc """
  An ETS-based cache for guilds.

  The supervisor defined by this module will set up the ETS table associated
  with it.

  The default table name under which guilds are cached is `#{@table_name}`.
  In addition to the cache behaviour implementations provided by this module,
  you can also call regular ETS table methods on it, such as `:ets.info`.

  Note that users should not call the functions not related to this specific
  implementation of the cache directly. Instead, call the functions of
  `Nostrum.Cache.GuildCache` directly, which will dispatch to the configured
  cache.
  """
  @moduledoc since: "0.5.0"

  @behaviour Nostrum.Cache.GuildCache

  alias Nostrum.Cache.GuildCache
  alias Nostrum.Cache.Mapping.ChannelGuild
  alias Nostrum.Snowflake
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Message
  alias Nostrum.Util
  import Nostrum.Snowflake, only: [is_snowflake: 1]
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the cache's ETS table."
  @impl Supervisor
  def init(_init_arg) do
    :ets.new(tabname(), [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  defguardp is_selector(term) when is_function(term, 1)

  @doc "Retrieve the ETS table name used for the cache."
  @spec tabname :: atom()
  def tabname, do: @table_name

  @doc "Retrieve all guilds from the cache."
  @impl GuildCache
  @spec all() :: Enum.t()
  def all do
    @table_name
    |> :ets.tab2list()
    |> Stream.map(&elem(&1, 1))
  end

  @doc "Retrieve all guilds matching the given selector from the cache."
  @impl GuildCache
  @spec select_all(GuildCache.selector()) :: Enum.t()
  def select_all(selector)

  def select_all(selector) when is_selector(selector) do
    :ets.foldl(fn {_id, guild}, acc -> [selector.(guild) | acc] end, [], @table_name)
  end

  @doc "Retrieve a guild from the cache by ID."
  @impl GuildCache
  @spec get(Guild.id()) :: {:ok, Guild.t()} | {:error, GuildCache.reason()}
  def get(id) do
    select(id, fn guild -> guild end)
  end

  @doc "Get a guild from the cache using the given selectors."
  @impl GuildCache
  @spec get_by(GuildCache.clauses()) :: {:ok, Guild.t()} | {:error, GuildCache.reason()}
  def get_by(clauses) do
    select_by(clauses, fn guild -> guild end)
  end

  @doc "Select values from the guild with the matching ID."
  @impl GuildCache
  @spec select(Guild.id(), GuildCache.selector()) :: {:ok, any} | {:error, GuildCache.reason()}
  def select(id, selector) do
    select_by(%{id: id}, selector)
  end

  @doc "Select values using a `selector` for a guild that matches the given `clauses`."
  @impl GuildCache
  @spec select_by(GuildCache.clauses(), GuildCache.selector()) ::
          {:ok, any} | {:error, GuildCache.reason()}
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

  # IMPLEMENTATION
  @doc "Create the given guild in the cache."
  @impl GuildCache
  @spec create(Guild.t()) :: true
  def create(guild) do
    # A duplicate guild insert is treated as a replace.
    true = :ets.insert(@table_name, {guild.id, guild})
  end

  @doc "Update the given guild in the cache."
  @impl GuildCache
  @spec update(map()) :: {Guild.t(), Guild.t()}
  def update(payload) do
    [{_id, old_guild}] = :ets.lookup(@table_name, payload.id)
    casted = Util.cast(payload, {:struct, Guild})
    new_guild = Guild.merge(old_guild, casted)
    true = :ets.update_element(@table_name, payload.id, {2, new_guild})
    {old_guild, new_guild}
  end

  @doc "Delete the given guild from the cache."
  @impl GuildCache
  @spec delete(Guild.id()) :: Guild.t() | nil
  def delete(guild_id) do
    # Returns the old guild, if cached
    case :ets.take(@table_name, guild_id) do
      [{_id, guild}] -> guild
      [] -> nil
    end
  end

  @doc "Create the given channel for the given guild in the cache."
  @impl GuildCache
  @spec channel_create(Guild.id(), map()) :: Channel.t()
  def channel_create(guild_id, channel) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    new_channel = Util.cast(channel, {:struct, Channel})
    new_channels = Map.put(guild.channels, channel.id, new_channel)
    new_guild = %{guild | channels: new_channels}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    new_channel
  end

  @doc "Delete the channel from the given guild in the cache."
  @impl GuildCache
  @spec channel_delete(Guild.id(), Channel.id()) :: Channel.t() | :noop
  def channel_delete(guild_id, channel_id) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {popped, new_channels} = Map.pop(guild.channels, channel_id)
    new_guild = %{guild | channels: new_channels}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    if popped, do: popped, else: :noop
  end

  @doc "Update the channel on the given guild in the cache."
  @impl GuildCache
  @spec channel_update(Guild.id(), map()) :: {Channel.t(), Channel.t()}
  def channel_update(guild_id, channel) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {old, new, new_channels} = upsert(guild.channels, channel.id, channel, Channel)
    new_guild = %{guild | channels: new_channels}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {old, new}
  end

  @doc "Update the emoji list for the given guild in the cache."
  @impl GuildCache
  @spec emoji_update(Guild.id(), [map()]) :: {[Emoji.t()], [Emoji.t()]}
  def emoji_update(guild_id, emojis) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    casted = Util.cast(emojis, {:list, {:struct, Emoji}})
    new = %{guild | emojis: casted}
    true = :ets.update_element(@table_name, guild_id, {2, new})
    {guild.emojis, casted}
  end

  @doc "Create the given role in the given guild in the cache."
  @impl GuildCache
  @spec role_create(Guild.id(), map()) :: {Guild.id(), Role.t()}
  def role_create(guild_id, role) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {_old, new, new_roles} = upsert(guild.roles, role.id, role, Role)
    new_guild = %{guild | roles: new_roles}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {guild_id, new}
  end

  @doc "Delete the given role from the given guild in the cache."
  @impl GuildCache
  @spec role_delete(Guild.id(), Role.id()) :: {Guild.id(), Role.t()} | :noop
  def role_delete(guild_id, role_id) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {popped, new_roles} = Map.pop(guild.roles, role_id)
    new_guild = %{guild | roles: new_roles}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    if popped, do: {guild_id, popped}, else: :noop
  end

  @doc "Update the given role in the given guild in the cache."
  @impl GuildCache
  @spec role_update(Guild.id(), map()) :: {Guild.id(), Role.t(), Role.t()}
  def role_update(guild_id, role) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    {old, new_role, new_roles} = upsert(guild.roles, role.id, role, Role)
    new_guild = %{guild | roles: new_roles}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {guild_id, old, new_role}
  end

  @doc "Update guild voice states with the given voice state in the cache."
  @impl GuildCache
  @spec voice_state_update(Guild.id(), map()) :: {Guild.id(), [map()]}
  def voice_state_update(guild_id, payload) do
    [{_id, guild}] = :ets.lookup(@table_name, guild_id)
    # Trim the `member` from the update payload.
    # Remove both `"member"` and `:member` in case of future key changes.
    trimmed_update = Map.drop(payload, [:member, "member"])
    state_without_user = Enum.reject(guild.voice_states, &(&1.user_id == trimmed_update.user_id))
    # If the `channel_id` is nil, then the user is leaving.
    # Otherwise, the voice state was updated.
    new_state =
      if(is_nil(trimmed_update.channel_id),
        do: state_without_user,
        else: [trimmed_update | state_without_user]
      )

    new_guild = %{guild | voice_states: new_state}
    true = :ets.update_element(@table_name, guild_id, {2, new_guild})
    {guild_id, new_state}
  end

  @doc "Increment the guild member count by one."
  @doc since: "0.7.0"
  @impl GuildCache
  @spec member_count_up(Guild.id()) :: true
  def member_count_up(guild_id) do
    [{^guild_id, guild}] = :ets.lookup(@table_name, guild_id)
    :ets.insert(@table_name, {guild_id, %{guild | member_count: guild.member_count + 1}})
  end

  @doc "Decrement the guild member count by one."
  @doc since: "0.7.0"
  @impl GuildCache
  @spec member_count_down(Guild.id()) :: true
  def member_count_down(guild_id) do
    [{^guild_id, guild}] = :ets.lookup(@table_name, guild_id)
    :ets.insert(@table_name, {guild_id, %{guild | member_count: guild.member_count - 1}})
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
