defmodule Nostrum.Cache.GuildCache.NoOp do
  @moduledoc """
  A cache module that does nothing.

  Useful for bots that don't need to cache guilds.
  """
  @moduledoc since: "0.6.0"
  @behaviour Nostrum.Cache.GuildCache

  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Util
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Start up the cache supervisor."
  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl GuildCache
  def all, do: []

  @impl GuildCache
  def select_all(_selector), do: []

  @impl GuildCache
  def get(_guild_id), do: {:error, :id_not_found_on_guild_lookup}

  @impl GuildCache
  def get_by(_clauses), do: {:error, :id_not_found_on_guild_lookup}

  @impl GuildCache
  def select(_guild_id, _selector), do: {:error, :id_not_found_on_guild_lookup}

  @impl GuildCache
  def select_by(_clauses, _selector), do: {:error, :id_not_found_on_guild_lookup}

  @impl GuildCache
  def create(_guild), do: true

  @impl GuildCache
  def update(guild) do
    guild = Util.cast(guild, {:struct, Guild})
    {guild, guild}
  end

  @impl GuildCache
  def delete(_guild_id), do: nil

  @impl GuildCache
  def channel_create(_guild_id, channel), do: Util.cast(channel, {:struct, Channel})

  @impl GuildCache
  def channel_delete(_guild_id, _channel_id), do: :noop

  @impl GuildCache
  def channel_update(_guild_id, channel) do
    channel = Util.cast(channel, {:struct, Channel})
    {channel, channel}
  end

  @impl GuildCache
  def emoji_update(_guild_id, emojis) do
    casted = Util.cast(emojis, {:list, {:struct, Emoji}})
    {[], casted}
  end

  @impl GuildCache
  def role_create(guild_id, role), do: {guild_id, Util.cast(role, {:struct, Role})}

  @impl GuildCache
  def role_update(guild_id, role) do
    role = Util.cast(role, {:struct, Role})
    {guild_id, role, role}
  end

  @impl GuildCache
  def role_delete(_guild_id, _role_id), do: :noop

  @impl GuildCache
  def voice_state_update(guild_id, _state), do: {guild_id, []}

  @impl GuildCache
  def member_count_up(_guild_id), do: true

  @impl GuildCache
  def member_count_down(_guild_id), do: true
end
