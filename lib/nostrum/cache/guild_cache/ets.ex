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
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Util
  use Supervisor

  require Logger

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

  @doc "Retrieve the ETS table name used for the cache."
  @spec tabname :: atom()
  def tabname, do: @table_name

  # IMPLEMENTATION
  @doc "Create the given guild in the cache."
  @impl GuildCache
  @spec create(map()) :: Guild.t()
  def create(payload) do
    guild = Guild.to_struct(payload)
    # A duplicate guild insert is treated as a replace.
    true = :ets.insert(@table_name, {guild.id, guild})
    guild
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

    {old, new, new_channels} =
      GuildCache.Base.upsert(guild.channels, channel.id, channel, Channel)

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
    {_old, new, new_roles} = GuildCache.Base.upsert(guild.roles, role.id, role, Role)
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
    {old, new_role, new_roles} = GuildCache.Base.upsert(guild.roles, role.id, role, Role)
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
    case :ets.lookup(@table_name, guild_id) do
      [{^guild_id, %{member_count: nil}}] ->
        # no-op if member_count is nil
        Logger.warning("Unexpected nil member_count for guild #{guild_id} when incrementing")
        true

      [{^guild_id, guild}] ->
        :ets.insert(@table_name, {guild_id, %{guild | member_count: guild.member_count + 1}})

      _ ->
        # Guilds caching disabled but member caching isn't
        true
    end
  end

  @doc "Decrement the guild member count by one."
  @doc since: "0.7.0"
  @impl GuildCache
  @spec member_count_down(Guild.id()) :: true
  def member_count_down(guild_id) do
    case :ets.lookup(@table_name, guild_id) do
      [{^guild_id, %{member_count: nil}}] ->
        # no-op if member_count is nil
        Logger.warning("Unexpected nil member_count for guild #{guild_id} when decrementing")
        true

      [{^guild_id, guild}] ->
        :ets.insert(@table_name, {guild_id, %{guild | member_count: guild.member_count - 1}})

      _ ->
        # Guilds caching disabled but member caching isn't
        true
    end
  end

  @impl GuildCache
  @doc "Get a QLC query handle for the guild cache."
  @doc since: "0.8.0"
  @spec query_handle :: :qlc.query_handle()
  def query_handle do
    :ets.table(@table_name)
  end
end
