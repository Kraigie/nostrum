% Native QLC operations.
%
% Using QLC from Elixir we pay the price of having to recompile our query
% handle every time we run it. For longer scans like `reduce` this is less of a
% problem, but for queries that users expect to be fast, like a `get` from an
% ETS table, more than a millisecond is unacceptable.
%
% Apart from the recompilation price, queries written using QLC's
% `string_to_handle` also have worse performance than queries written in native
% Erlang, see
% https://elixirforum.com/t/performance-discrepancies-with-using-qlc-queries-written-in-erlang-and-elixir/56006.
% I assume this is caused by the Erlang parse transform doing smart things at compile time.

-module(nostrum_member_cache_qlc).
-export([by_user/2, by_guild/2, lookup/3, get_with_users/3]).

-include_lib("stdlib/include/qlc.hrl").

% The matching on the cache names here is smelly. But we need it for the
% built-in caches for now.
%
% Implement https://github.com/erlang/otp/issues/7268 to improve it.


-spec by_user('Elixir.Nostrum.Struct.User':id(), module()) -> qlc:query_handle().
by_user(UserId, Cache) ->
    QH = case erlang:function_exported(Cache, query_handle, 1) of
             true -> Cache:query_handle([{'==', '$2', {const, UserId}}]);
             false -> Cache:query_handle()
         end,

    qlc:q([{GuildId, Member} || {{GuildId, MemberId}, Member} <- QH,
                                MemberId =:= UserId]).

-spec by_guild('Elixir.Nostrum.Struct.Guild':id(), module()) -> qlc:query_handle().
by_guild(RequestedGuildId, Cache) ->
    QH = case erlang:function_exported(Cache, query_handle, 1) of
             true -> Cache:query_handle([{'==', '$1', {const, RequestedGuildId}}]);
             false -> Cache:query_handle()
         end,

    qlc:q([Member || {{GuildId, _MemberId}, Member} <- QH,
                     GuildId =:= RequestedGuildId]).

-spec by_guild_with_user_id('Elixir.Nostrum.Struct.Guild':id(), module()) -> qlc:query_handle().
by_guild_with_user_id(RequestedGuildId, Cache) ->
    QH = case erlang:function_exported(Cache, query_handle, 1) of
             true -> Cache:query_handle([{'==', '$1', {const, RequestedGuildId}}]);
             false -> Cache:query_handle()
         end,

    qlc:q([{MemberId, Member} || {{GuildId, MemberId}, Member} <- QH,
                                 GuildId =:= RequestedGuildId]).

-spec lookup('Elixir.Nostrum.Struct.Guild':id(), 'Elixir.Nostrum.Struct.User':id(), module()) -> qlc:query_handle().
lookup(RequestedGuildId, RequestedUserId, Cache) ->
    QH = case erlang:function_exported(Cache, query_handle, 1) of
             true ->
                 Cache:query_handle([{'==', '$1', {const, RequestedGuildId}},
                                     {'==', '$2', {const, RequestedUserId}}]);
             false -> Cache:query_handle()
         end,

    qlc:q([Member || {{GuildId, MemberId}, Member} <- QH,
                     GuildId =:= RequestedGuildId,
                     MemberId =:= RequestedUserId]).


-spec get_with_users('Elixir.Nostrum.Struct.Guild':id(), module(), module()) -> qlc:query_handle().
get_with_users(RequestedGuildId, MemberCache, UserCache) ->
    qlc:q([{Member, User} || {MemberId, Member} <- by_guild_with_user_id(RequestedGuildId, MemberCache),
                          {UserId, User} <- UserCache:query_handle(),
                          MemberId =:= UserId]).
