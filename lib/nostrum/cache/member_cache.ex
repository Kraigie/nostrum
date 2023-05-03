defmodule Nostrum.Cache.MemberCache do
  @default_cache_implementation Nostrum.Cache.MemberCache.ETS
  @moduledoc """
  Cache behaviour & dispatcher for guild members.

  You can call the functions provided by this module independent of which cache
  is configured, and it will dispatch to the configured cache implementation.

  By default, #{@default_cache_implementation} will be used for caching
  members. You can override this in the `:caches` option of the `:nostrum`
  application by setting the `:members` field to a different module
  implementing the behaviour defined by this module.

  See the documentation for the `Nostrum.Cache.GuildCache` module for more
  details on how to implement your own.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Cache.UserCache
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User

  @configured_cache :nostrum
                    |> Application.compile_env([:caches, :members], @default_cache_implementation)

  @doc """
  Get members for a given guild ID.

  The result is returned as a stream to accomodate for large guilds.
  """
  @callback get(Guild.id()) :: Enumerable.t(Member.t())

  @doc """
  Get a single member on the given guild ID.

  The result should be returned as a stream to accomodate for large guilds.
  """
  @callback get(Guild.id(), Member.user_id()) :: {:ok, Member.t()} | {:error, atom()}

  @doc """
  Get all members cached for the given user ID.

  The members will be returned alongside their guild ID as a pair in the
  format `{guild_id, member}`.

  The result is returned as a stream.
  """
  @callback by_user(Member.user_id()) :: Enumerable.t({Guild.id(), Member.t()})

  @doc """
  Add the member for the given guild from upstream data.

  Return the casted member structure.
  """
  @callback create(Guild.id(), member :: map()) :: Member.t()

  @doc """
  Update the given member for the given guild from upstream data.

  Return the guild ID that was updated, the old cached member (if the member
  was known to the cache), and the updated member.

  ## Note regarding intents

  Even if the required intents to receive `GUILD_MEMBER_UPDATE` events are
  disabled to a point where we do not receive guild creation events, it is
  still possible to receive the event for our own user. An example of this can
  be found in [issue
  #293](https://github.com/Kraigie/nostrum/issues/293). Note that the issue
  predates the modern nostrum caching infrastructure.
  """
  @callback update(Guild.id(), member :: map()) ::
              {Guild.id(), old_member :: Member.t() | nil, updated_member :: Member.t()}

  @doc """
  Remove the given user for the given guild.

  Return the guild ID and old member if the member was cached. Otherwise,
  return `:noop`.
  """
  @callback delete(Guild.id(), user :: map()) ::
              {Guild.id(), old_member :: Member.t()} | :noop

  @doc """
  Bulk create multiple members in the cache from upstream data.

  Return value is unused, as we currently do not dispatch a gateway for this.
  """
  @callback bulk_create(Guild.id(), members :: [member :: map()]) :: true

  @doc """
  Return a query handle for usage with `:qlc`.

  This is used by nostrum to provide automatic joins between the member and the
  user cache, and may be used for other functions in the future.

  The Erlang manual on [Implementing a QLC
  Table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table)
  contains examples for implementation.

  The query handle must return items in the form `{guild_id, user_id,
  member}`, where:
  - `guild_id` is a `t:Nostrum.Struct.Guild.id/0`,
  - `user_id` is a `t:Nostrum.Struct.User.id/0`, and
  - `member` is a `t:Nostrum.Struct.Guild.Member.t/0`.
  """
  @callback qlc_handle() :: :qlc.query_handle()

  # User-facing
  defdelegate get(guild_id), to: @configured_cache
  defdelegate get(guild_id, member_id), to: @configured_cache
  defdelegate by_user(member_id), to: @configured_cache

  @doc """
  Return an enumerable of members with their users.

  ## Parameters

  - `guild_id` (`t:Nostrum.Struct.Guild.id/0`): The guild for which to return members.

  ## Return value

  Returns an enumerable that can be consumed with any function in the `Stream`
  or `Enumerable` module.

  If the user for a guild member is not found, the member and user won't be
  present in the result. Barring a bug in nostrum's caching, this should never
  happen in practice.
  """
  @spec get_with_users(Guild.id()) :: Enumerable.t({Member.t(), User.t()})
  def get_with_users(guild_id) do
    member_query_handle = qlc_handle()
    user_query_handle = UserCache.qlc_handle()
    initial_bindings = :erl_eval.new_bindings()
    member_bindings = :erl_eval.add_binding(:MemberHandle, member_query_handle, initial_bindings)
    user_bindings = :erl_eval.add_binding(:UserHandle, user_query_handle, member_bindings)
    final_bindings = :erl_eval.add_binding(:RequestedGuildId, guild_id, user_bindings)

    joined_handle =
      :qlc.string_to_handle(
        '[{Member, User} || ' ++
          '{GuildId, MemberId, Member} <- MemberHandle, ' ++
          '{UserId, User} <- UserHandle, ' ++
          'GuildId =:= RequestedGuildId, ' ++
          'UserId =:= MemberId].',
        [],
        final_bindings
      )

    Stream.resource(
      fn -> :qlc.cursor(joined_handle) end,
      fn cursor -> cursor |> :qlc.next_answers(100) |> parse_qlc_answers(cursor) end,
      fn cursor -> :qlc.delete_cursor(cursor) end
    )
  end

  defp parse_qlc_answers([], cursor) do
    {:halt, cursor}
  end

  defp parse_qlc_answers(answers, cursor) do
    parsed_answers = Enum.map(answers, fn {member, user} -> {member, User.to_struct(user)} end)
    {parsed_answers, cursor}
  end

  # Nostrum-facing
  @doc false
  defdelegate create(guild_id, member), to: @configured_cache
  @doc false
  defdelegate delete(guild_id, user), to: @configured_cache
  @doc false
  defdelegate update(guild_id, member), to: @configured_cache
  @doc false
  defdelegate bulk_create(guild_id, members), to: @configured_cache
  @doc false
  defdelegate qlc_handle(), to: @configured_cache

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate init(init_arg), to: @configured_cache
  @doc false
  defdelegate start_link(init_arg), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
