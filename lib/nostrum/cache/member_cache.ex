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
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Cache.UserCache
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User

  @configured_cache :nostrum
                    |> Application.compile_env([:caches, :members], @default_cache_implementation)

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
  Return a QLC query handle for cache read operations.

  This is used by nostrum to provide any read operations on the cache. Write
  operations still need to be implemented separately.

  The Erlang manual on [Implementing a QLC
  Table](https://www.erlang.org/doc/man/qlc.html#implementing_a_qlc_table)
  contains examples for implementation. To prevent full table scans, accept
  match specifications in your `TraverseFun` and implement a `LookupFun` as
  documented.

  The query handle must return items in the form `{guild_id, user_id,
  member}`, where:
  - `guild_id` is a `t:Nostrum.Struct.Guild.id/0`,
  - `user_id` is a `t:Nostrum.Struct.User.id/0`, and
  - `member` is a `t:Nostrum.Struct.Guild.Member.t/0`.

  If your cache needs some form of setup or teardown for QLC queries (such as
  opening connections), see `c:wrap_qlc/1`.
  """
  @callback query_handle() :: :qlc.query_handle()

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  A function that should wrap any `:qlc` operations.

  If you implement a cache that is backed by a database and want to perform
  cleanup and teardown actions such as opening and closing connections,
  managing transactions and so on, you want to implement this function. nostrum
  will then effectively call `wrap_qlc(fn -> :qlc.e(...) end)`.

  If your cache does not need any wrapping, you can omit this.
  """
  @doc since: "0.8.0"
  @callback wrap_qlc((-> result)) :: result when result: term()
  @optional_callbacks wrap_qlc: 1

  # User-facing

  @doc """
  Return a member together with its user via the user cache.
  """
  @spec get_with_user(Guild.id(), Member.user_id()) :: {Member.t(), User.t() | nil} | nil
  @spec get_with_user(Guild.id(), Member.user_id(), module()) ::
          {Member.t(), User.t() | nil} | nil
  def get_with_user(guild_id, member_id, cache \\ @configured_cache) do
    case get(guild_id, member_id, cache) do
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
  """
  @doc since: "0.8.0"
  @spec fold_by_user(acc, Member.user_id(), ({Guild.id(), Member.t()}, acc -> acc)) :: acc
        when acc: term()
  @spec fold_by_user(acc, Member.user_id(), ({Guild.id(), Member.t()}, acc -> acc), module()) ::
          acc
        when acc: term()
  def fold_by_user(acc, user_id, member_reducer, cache \\ @configured_cache) do
    handle = :nostrum_member_cache_qlc.by_user(user_id, cache)
    wrap_qlc(cache, fn -> :qlc.fold(member_reducer, acc, handle) end)
  end

  @doc """
  Get a single member on the given guild ID.
  """
  @spec get(Guild.id(), Member.user_id()) :: {:ok, Member.t()} | {:error, atom()}
  @spec get(Guild.id(), Member.user_id(), module()) :: {:ok, Member.t()} | {:error, atom()}
  def get(guild_id, user_id, cache \\ @configured_cache) do
    handle = :nostrum_member_cache_qlc.lookup(guild_id, user_id, cache)

    wrap_qlc(cache, fn ->
      case :qlc.e(handle) do
        [result] ->
          {:ok, result}

        [] ->
          {:error, :not_found}
      end
    end)
  end

  @doc """
  Fold (reduce) over members for the given guild ID.

  ## Parameters

  - `acc`: The initial accumulator. Also returned if no guild members were found.
  - `guild_id`: The guild for which to reduce members.
  - `fun`: Called for every element in the result. Takes a pair
  in the form `{member, acc)`, and must return the updated accumulator.

  ## Return value

  Returns the resulting accumulator via `fun`. Returns `acc` unchanged if no
  results were found.
  """
  @doc since: "0.8.0"
  @spec fold(acc, Guild.id(), (Member.t(), acc -> acc)) :: acc when acc: term()
  @spec fold(acc, Guild.id(), (Member.t(), acc -> acc), module()) :: acc when acc: term()
  def fold(acc, guild_id, member_reducer, cache \\ @configured_cache) do
    handle = :nostrum_member_cache_qlc.by_guild(guild_id, cache)
    wrap_qlc(cache, fn -> :qlc.fold(member_reducer, acc, handle) end)
  end

  @doc """
  Calls `fun` on each member and its user on the given guild ID, with the given
  accumulator.

  ## Parameters

  - `acc` (`term()`): The initial accumulator. Also returned if no guild
  members were found.
  - `guild_id` (`t:Nostrum.Struct.Guild.id/0`): The guild for which to reduce members.
  - `fun` (`function()`): Called for every element in the result. Takes a pair
  in the form `{{member, user}, acc)`, and must return the updated accumulator.

  ## Return value

  Returns the resulting accumulator via `fun`. Returns `acc` unchanged if no
  results were found.

  If the user for a guild member is not found, the member _and_ user won't be
  present in the result. Barring a bug in nostrum's caching, this should never
  happen in practice.
  """
  @doc since: "0.8.0"
  @spec fold_with_users(acc, Guild.id(), ({Member.t(), User.t()}, acc -> acc)) :: acc
        when acc: term()
  @spec fold_with_users(acc, Guild.id(), ({Member.t(), User.t()}, acc -> acc), module()) ::
          acc
        when acc: term()
  def fold_with_users(acc, guild_id, fun, cache \\ @configured_cache) do
    joined_handle = :nostrum_member_cache_qlc.get_with_users(guild_id, cache, UserCache)

    wrapped_fun = fn {member, user}, acc ->
      fun.({member, user}, acc)
    end

    wrap_qlc(cache, fn ->
      UserCache.wrap_qlc(fn ->
        :qlc.fold(wrapped_fun, acc, joined_handle)
      end)
    end)
  end

  @doc """
  Call `c:wrap_qlc/1` on the given cache, if implemented.

  If no cache is given, calls out to the default cache.
  """
  @doc since: "0.8.0"
  @spec wrap_qlc((-> result)) :: result when result: term()
  @spec wrap_qlc(module(), (-> result)) :: result when result: term()
  def wrap_qlc(cache \\ @configured_cache, fun) do
    if function_exported?(cache, :wrap_qlc, 1) do
      cache.wrap_qlc(fun)
    else
      fun.()
    end
  end

  # Nostrum-facing
  @doc false
  defdelegate create(guild_id, member), to: @configured_cache
  @doc false
  defdelegate delete(guild_id, user_id), to: @configured_cache
  @doc false
  defdelegate update(guild_id, member), to: @configured_cache
  @doc false
  defdelegate bulk_create(guild_id, members), to: @configured_cache

  @doc """
  Return the QLC handle of the configured cache.
  """
  @doc since: "0.8.0"
  defdelegate query_handle(), to: @configured_cache

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
