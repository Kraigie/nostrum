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

  The user-facing functions of this module can be called with a custom cache as
  the final argument. This is mainly useful if you want to test the cache: by
  default, nostrum will use #{@default_cache_implementation}.

  In order to use this cache, you must enable the `:guild_members` intent, and you must also enable the `request_guild_members: true` configuration option.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Cache.UserCache
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User

  @configured_cache Nostrum.Cache.Base.get_cache_module(:members, @default_cache_implementation)

  @doc """
  Retrieve a member from the cache by guild and user ID.
  """
  @doc since: "0.10.0"
  @callback get(Guild.id(), Member.user_id()) :: {:ok, Member.t()} | {:error, atom()}

  @doc """
  Yield an enumerable of `{guild_id, member}` pairs associated with the given user ID.

  Since nostrum does not know when it can stop streaming you results, you have
  to wrap calls to this function in `wrap_query/1`.
  """
  @doc since: "0.10.0"
  @callback by_user(User.id()) :: Enumerable.t({Guild.id(), Member.t()})

  @doc """
  Yield an enumerable of members associated with the given guild ID.
  """
  @doc since: "0.10.0"
  @callback by_guild(Guild.id()) :: Enumerable.t(Member.t())

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
  @callback delete(Guild.id(), Member.user_id()) ::
              {Guild.id(), old_member :: Member.t()} | :noop

  @doc """
  Bulk create multiple members in the cache from upstream data.

  Return value is unused, as we currently do not dispatch a gateway for this.
  """
  @callback bulk_create(Guild.id(), members :: [member :: map()]) :: true

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  A function that should wrap any long-running query operations.

  If you implement a cache that is backed by a database and want to perform
  cleanup and teardown actions such as opening and closing connections,
  managing transactions and so on, you want to implement this function. nostrum
  will then effectively call `wrap_query(fn -> ... end)`.

  If your cache does not need any wrapping, you can omit this.
  """
  @doc since: "0.10.0"
  @callback wrap_query((-> result)) :: result when result: term()
  @optional_callbacks wrap_query: 1

  # User-facing

  @doc """
  Return a member together with its user via the user cache.
  """
  @spec get_with_user(Guild.id(), Member.user_id()) :: {Member.t(), User.t() | nil} | nil
  @spec get_with_user(Guild.id(), Member.user_id(), module()) ::
          {Member.t(), User.t() | nil} | nil
  def get_with_user(guild_id, member_id, cache \\ @configured_cache) do
    case cache.get(guild_id, member_id) do
      {:ok, member} ->
        case UserCache.get(member_id) do
          {:ok, user} ->
            {member, user}

          _ ->
            {member, nil}
        end

      _ ->
        nil
    end
  end

  @doc """
  Reduce over all members cached for the given user ID.

  The members will be returned alongside their guild ID as a pair in the
  format `{guild_id, member}`.

  ## Raises

  Fails when `by_user/1` of the selected cache returns an error.
  """
  @doc since: "0.8.0"
  @spec fold_by_user(acc, Member.user_id(), ({Guild.id(), Member.t()}, acc -> acc)) :: acc
        when acc: term()
  @spec fold_by_user(acc, Member.user_id(), ({Guild.id(), Member.t()}, acc -> acc), module()) ::
          acc
        when acc: term()
  def fold_by_user(acc, user_id, member_reducer, cache \\ @configured_cache) do
    enumerable = cache.by_user(user_id)
    wrap_query(cache, fn -> Enum.reduce(enumerable, acc, member_reducer) end)
  end

  @doc """
  Get a single member on the given guild ID.
  """
  @spec get(Guild.id(), Member.user_id()) :: {:ok, Member.t()} | {:error, atom()}
  defdelegate get(guild_id, member_id), to: @configured_cache

  @doc """
  Fold (reduce) over members for the given guild ID.

  ## Parameters

  - `acc`: The initial accumulator. Also returned if no guild members were found.
  - `guild_id`: The guild for which to reduce members.
  - `member_reducer`: Called for every element in the result. Takes a pair
  in the form `(member, acc)`, and must return the updated accumulator.

  ## Return value

  Returns the resulting accumulator via `fun`. Returns `acc` unchanged if no
  results were found.

  ## Raises

  Fails when `by_guild/1` of the selected cache returns an error.
  """
  @doc since: "0.8.0"
  @spec fold(acc, Guild.id(), (Member.t(), acc -> acc)) :: acc when acc: term()
  @spec fold(acc, Guild.id(), (Member.t(), acc -> acc), module()) :: acc when acc: term()
  def fold(acc, guild_id, member_reducer, cache \\ @configured_cache) do
    enumerable = cache.by_guild(guild_id)
    wrap_query(cache, fn -> Enum.reduce(enumerable, acc, member_reducer) end)
  end

  @doc """
  Calls `fun` on each member and its user on the given guild ID, with the given
  accumulator.

  ## Parameters

  - `acc` (`term()`): The initial accumulator. Also returned if no guild
  members were found.
  - `guild_id` (`t:Nostrum.Struct.Guild.id/0`): The guild for which to reduce members.
  - `fun` (`function()`): Called for every element in the result. Takes a pair
  in the form `({member, user}, acc)`, and must return the updated accumulator.

  ## Return value

  Returns the resulting accumulator via `fun`. Returns `acc` unchanged if no
  results were found.

  If the user for a guild member is not found, the member _and_ user won't be
  present in the result. Barring a bug in nostrum's caching, this should never
  happen in practice.

  ## Raises

  Fails when `by_guild/1` of the selected cache returns an error.
  """
  @doc since: "0.8.0"
  @spec fold_with_users(acc, Guild.id(), ({Member.t(), User.t()}, acc -> acc)) :: acc
        when acc: term()
  @spec fold_with_users(acc, Guild.id(), ({Member.t(), User.t()}, acc -> acc), module()) ::
          acc
        when acc: term()
  def fold_with_users(acc, guild_id, fun, cache \\ @configured_cache) do
    enumerable = cache.by_guild(guild_id)

    wrap_query(cache, fn ->
      Enum.reduce(
        enumerable,
        acc,
        fn %Member{user_id: user_id} = member, acc ->
          # credo:disable-for-next-line
          case UserCache.get(user_id) do
            {:ok, user} -> fun.({member, user}, acc)
            _error -> acc
          end
        end
      )
    end)
  end

  @doc """
  Call `c:wrap_query/1` on the given cache, if implemented.

  If no cache is given, calls out to the default cache.
  """
  @doc since: "0.10.0"
  @spec wrap_query((-> result)) :: result when result: term()
  @spec wrap_query(module(), (-> result)) :: result when result: term()
  def wrap_query(cache \\ @configured_cache, fun) do
    if function_exported?(cache, :wrap_query, 1) do
      cache.wrap_query(fun)
    else
      fun.()
    end
  end

  defdelegate by_guild(guild_id), to: @configured_cache
  defdelegate by_user(user_id), to: @configured_cache

  # Nostrum-facing
  @doc false
  defdelegate create(guild_id, member), to: @configured_cache
  @doc false
  defdelegate delete(guild_id, user_id), to: @configured_cache
  @doc false
  defdelegate update(guild_id, member), to: @configured_cache
  @doc false
  defdelegate bulk_create(guild_id, members), to: @configured_cache

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
