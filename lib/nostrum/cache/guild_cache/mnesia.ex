if Code.ensure_loaded?(:mnesia) do
  defmodule Nostrum.Cache.GuildCache.Mnesia do
    @moduledoc """
    An Mnesia-based cache for guilds.

    #{Nostrum.Cache.Base.mnesia_note()}
    """
    @moduledoc since: "0.8.0"

    @table_name :nostrum_guilds
    @record_name @table_name

    @behaviour Nostrum.Cache.GuildCache

    alias Nostrum.Cache.GuildCache
    alias Nostrum.Struct.Channel
    alias Nostrum.Struct.Emoji
    alias Nostrum.Struct.Guild
    alias Nostrum.Struct.Guild.Role
    alias Nostrum.Struct.Sticker
    alias Nostrum.Util
    use Supervisor

    @doc "Start the supervisor."
    def start_link(init_arg) do
      Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
    end

    @impl Supervisor
    @doc "Set up the cache's Mnesia table."
    def init(_init_arg) do
      options = [
        attributes: [:id, :data],
        record_name: @record_name
      ]

      case :mnesia.create_table(@table_name, options) do
        {:atomic, :ok} -> :ok
        {:aborted, {:already_exists, _tab}} -> :ok
      end

      Supervisor.init([], strategy: :one_for_one)
    end

    @doc "Retrieve the Mnesia table name used for the cache."
    @spec table :: atom()
    def table, do: @table_name

    @doc "Drop the table used for caching."
    @spec teardown() :: {:atomic, :ok} | {:aborted, term()}
    def teardown, do: :mnesia.delete_table(@table_name)

    @doc "Clear any objects in the cache."
    @spec clear() :: :ok
    def clear do
      {:atomic, :ok} = :mnesia.clear_table(@table_name)
      :ok
    end

    @impl GuildCache
    @doc "Get a guild from the cache."
    @spec get(Guild.id()) :: {:ok, Guild.t()} | {:error, :not_found}
    def get(guild_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, guild_id) do
          [{_tag, _id, guild}] -> {:ok, guild}
          [] -> {:error, :not_found}
        end
      end)
    end

    @impl GuildCache
    @doc since: "0.10.0"
    @spec all() :: Enumerable.t(Guild.t())
    def all do
      ms = [{{:_, :_, :"$1"}, [], [:"$1"]}]

      Stream.resource(
        fn -> :mnesia.select(@table_name, ms, 100, :read) end,
        fn items ->
          case items do
            {matches, cont} ->
              {matches, :mnesia.select(cont)}

            :"$end_of_table" ->
              {:halt, nil}
          end
        end,
        fn _cont -> :ok end
      )
    end

    # Used by dispatch

    @impl GuildCache
    @doc "Create a guild from upstream data."
    @spec create(map()) :: Guild.t()
    def create(payload) do
      guild = Guild.to_struct(payload)
      record = {@record_name, guild.id, guild}
      writer = fn -> :mnesia.write(record) end
      :ok = :mnesia.activity(:sync_transaction, writer)
      guild
    end

    @impl GuildCache
    @doc "Update the given guild in the cache."
    @spec update(map()) :: {Guild.t() | nil, Guild.t()}
    def update(payload) do
      new_guild = Guild.to_struct(payload)

      old_guild =
        :mnesia.activity(:sync_transaction, fn ->
          case :mnesia.read(@table_name, new_guild.id, :write) do
            [{_tag, _id, old_guild} = entry] ->
              updated_guild = Guild.merge(old_guild, new_guild)

              :mnesia.write(put_elem(entry, 2, updated_guild))
              old_guild

            [] ->
              nil
          end
        end)

      {old_guild, new_guild}
    end

    @impl GuildCache
    @doc "Remove the given guild from the cache."
    @spec delete(Guild.id()) :: Guild.t() | nil
    def delete(guild_id) do
      :mnesia.activity(:sync_transaction, fn ->
        case :mnesia.read(@table_name, guild_id, :write) do
          [{_tag, _guild_id, guild}] ->
            :mnesia.delete(@table_name, guild_id, :write)
            guild

          _ ->
            nil
        end
      end)
    end

    @impl GuildCache
    @doc "Create the given channel for the given guild in the cache."
    @spec channel_create(Guild.id(), map()) :: Channel.t()
    def channel_create(guild_id, channel) do
      new_channel = Channel.to_struct(channel)

      update_guild!(guild_id, fn guild ->
        new_channels = Map.put(guild.channels, channel.id, new_channel)
        {%{guild | channels: new_channels}, :ok}
      end)

      new_channel
    end

    @impl GuildCache
    @doc "Delete the channel from the given guild in the cache."
    @spec channel_delete(Guild.id(), Channel.id()) :: Channel.t() | :noop
    def channel_delete(guild_id, channel_id) do
      old_channel =
        update_guild!(guild_id, fn guild ->
          {popped, new_channels} = Map.pop(guild.channels, channel_id)
          {%{guild | channels: new_channels}, popped}
        end)

      if old_channel, do: old_channel, else: :noop
    end

    @doc "Update the channel on the given guild in the cache."
    @impl GuildCache
    @spec channel_update(Guild.id(), map()) :: {Channel.t() | nil, Channel.t()}
    def channel_update(guild_id, channel) do
      update_guild!(guild_id, fn guild ->
        {old, new, new_channels} =
          GuildCache.Base.upsert(guild.channels, channel.id, channel, Channel)

        {%{guild | channels: new_channels}, {old, new}}
      end)
    end

    @impl GuildCache
    @doc "Update the emoji list for the given guild in the cache."
    @spec emoji_update(Guild.id(), [map()]) :: {[Emoji.t()], [Emoji.t()]}
    def emoji_update(guild_id, payload) do
      new_emojis = Util.cast(payload, {:list, {:struct, Emoji}})

      old_emojis =
        update_guild!(guild_id, fn guild ->
          {%{guild | emojis: new_emojis}, guild.emojis}
        end)

      {old_emojis, new_emojis}
    end

    @impl GuildCache
    @doc "Update the sticker list for the given guild in the cache."
    @spec stickers_update(Guild.id(), [map()]) :: {[Sticker.t()], [Sticker.t()]}
    def stickers_update(guild_id, stickers) do
      new_stickers = Util.cast(stickers, {:list, {:struct, Sticker}})

      old_stickers =
        update_guild!(guild_id, fn guild ->
          {%{guild | stickers: new_stickers}, guild.stickers}
        end)

      {old_stickers, new_stickers}
    end

    @impl GuildCache
    @doc "Create the given role in the given guild in the cache."
    @spec role_create(Guild.id(), map()) :: {Guild.id(), Role.t()}
    def role_create(guild_id, payload) do
      update_guild!(guild_id, fn guild ->
        {_old, new, new_roles} = GuildCache.Base.upsert(guild.roles, payload.id, payload, Role)
        {%{guild | roles: new_roles}, {guild_id, new}}
      end)
    end

    @doc "Delete the given role from the given guild in the cache."
    @impl GuildCache
    @spec role_delete(Guild.id(), Role.id()) :: {Guild.id(), Role.t()} | :noop
    def role_delete(guild_id, role_id) do
      update_guild!(guild_id, fn guild ->
        {popped, new_roles} = Map.pop(guild.roles, role_id)
        result = if popped, do: {guild_id, popped}, else: :noop
        {%{guild | roles: new_roles}, result}
      end)
    end

    @impl GuildCache
    @doc "Update the given role in the given guild in the cache."
    @spec role_update(Guild.id(), map()) :: {Guild.id(), Role.t() | nil, Role.t()}
    def role_update(guild_id, role) do
      update_guild!(guild_id, fn guild ->
        {old, new_role, new_roles} = GuildCache.Base.upsert(guild.roles, role.id, role, Role)
        new_guild = %{guild | roles: new_roles}
        {new_guild, {guild_id, old, new_role}}
      end)
    end

    @impl GuildCache
    @doc "Update guild voice states with the given voice state in the cache."
    @spec voice_state_update(Guild.id(), map()) :: {Guild.id(), [map()]}
    def voice_state_update(guild_id, payload) do
      trimmed_update = Map.drop(payload, [:member, "member"])

      update_guild!(guild_id, fn guild ->
        # Trim the `member` from the update payload.
        # Remove both `"member"` and `:member` in case of future key changes.
        state_without_user =
          Enum.reject(guild.voice_states, &(&1.user_id == trimmed_update.user_id))

        # If the `channel_id` is nil, then the user is leaving.
        # Otherwise, the voice state was updated.
        new_state =
          if(is_nil(trimmed_update.channel_id),
            do: state_without_user,
            else: [trimmed_update | state_without_user]
          )

        {%{guild | voice_states: new_state}, {guild_id, new_state}}
      end)
    end

    @doc "Increment the guild member count by one."
    @doc since: "0.7.0"
    @impl GuildCache
    @spec member_count_up(Guild.id()) :: true
    def member_count_up(guild_id) do
      # May not be `update_guild!` for the case where guild intent is off.
      update_guild(guild_id, fn guild ->
        {%{guild | member_count: guild.member_count + 1}, true}
      end)

      true
    end

    @doc "Decrement the guild member count by one."
    @doc since: "0.7.0"
    @impl GuildCache
    @spec member_count_down(Guild.id()) :: true
    def member_count_down(guild_id) do
      # May not be `update_guild!` for the case where guild intent is off.
      update_guild(guild_id, fn guild ->
        {%{guild | member_count: guild.member_count - 1}, true}
      end)

      true
    end

    defp update_guild(guild_id, updater) do
      :mnesia.activity(
        :sync_transaction,
        fn ->
          case :mnesia.read(@table_name, guild_id, :write) do
            [{_tag, _id, old_guild} = entry] ->
              {new_guild, result} = updater.(old_guild)
              :mnesia.write(put_elem(entry, 2, new_guild))
              result

            [] ->
              nil
          end
        end
      )
    end

    defp update_guild!(guild_id, updater) do
      :mnesia.activity(
        :sync_transaction,
        fn ->
          [{_tag, _id, old_guild} = entry] = :mnesia.read(@table_name, guild_id, :write)
          {new_guild, result} = updater.(old_guild)
          :mnesia.write(put_elem(entry, 2, new_guild))
          result
        end
      )
    end

    @impl GuildCache
    @doc "Wrap queries in a transaction."
    def wrap_query(fun) do
      :mnesia.activity(:sync_transaction, fun)
    end
  end
end
