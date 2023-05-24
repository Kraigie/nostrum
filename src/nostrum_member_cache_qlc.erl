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

-define(MNESIA_CACHE, 'Elixir.Nostrum.Cache.MemberCache.Mnesia').

% These must be selected carefully so that QLC can plan using the indices properly.
-define(MNESIA_FORMAT, {_Tag, {GuildId, MemberId}, GuildId, MemberId, Member}).
-define(MNESIA_FORMAT_NOMEMBERID, {_Tag, {_GuildId, _MemberId}, GuildId, _, Member}).


-spec by_user('Elixir.Nostrum.Struct.User':id(), module()) -> qlc:query_handle().
by_user(UserId, ?MNESIA_CACHE) ->
    qlc:q([{GuildId, Member} || ?MNESIA_FORMAT <- ?MNESIA_CACHE:query_handle(),
                                MemberId =:= UserId]);

by_user(UserId, Cache) ->
    qlc:q([{GuildId, Member} || {{GuildId, MemberId}, Member} <- Cache:query_handle(),
                                MemberId =:= UserId]).

-spec by_guild('Elixir.Nostrum.Struct.Guild':id(), module()) -> qlc:query_handle().
by_guild(RequestedGuildId, ?MNESIA_CACHE) ->
    qlc:q([Member || ?MNESIA_FORMAT_NOMEMBERID <- ?MNESIA_CACHE:query_handle(),
                     GuildId =:= RequestedGuildId]);

by_guild(RequestedGuildId, Cache) ->
    qlc:q([Member || {{GuildId, _MemberId}, Member} <- Cache:query_handle(),
                     GuildId =:= RequestedGuildId]).

-spec lookup('Elixir.Nostrum.Struct.Guild':id(), 'Elixir.Nostrum.Struct.User':id(), module()) -> qlc:query_handle().
lookup(RequestedGuildId, RequestedUserId, ?MNESIA_CACHE) ->
    qlc:q([Member || ?MNESIA_FORMAT <- ?MNESIA_CACHE:query_handle(),
                     GuildId =:= RequestedGuildId,
                     MemberId =:= RequestedUserId]);

lookup(RequestedGuildId, RequestedUserId, Cache) ->
    qlc:q([Member || {{GuildId, MemberId}, Member} <- Cache:query_handle(),
                     GuildId =:= RequestedGuildId,
                     MemberId =:= RequestedUserId]).


-spec get_with_users('Elixir.Nostrum.Struct.Guild':id(), module(), module()) -> qlc:query_handle().
get_with_users(RequestedGuildId, ?MNESIA_CACHE, UserCache) ->
    qlc:q([{Member, User} || ?MNESIA_FORMAT <- ?MNESIA_CACHE:query_handle(),
                          GuildId =:= RequestedGuildId,
                          {UserId, User} <- UserCache:query_handle(),
                          MemberId =:= UserId]);

get_with_users(RequestedGuildId, MemberCache, UserCache) ->
    qlc:q([{Member, User} || {{GuildId, MemberId}, Member} <- MemberCache:query_handle(),
                          GuildId =:= RequestedGuildId,
                          {UserId, User} <- UserCache:query_handle(),
                          MemberId =:= UserId]).
