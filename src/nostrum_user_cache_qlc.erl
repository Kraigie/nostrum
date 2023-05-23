-module(nostrum_user_cache_qlc).
-export([get/2]).

-include_lib("stdlib/include/qlc.hrl").

% Optimized user cache QLC queries.

% See https://github.com/erlang/otp/issues/7268
get(RequestedUserId, 'Elixir.Nostrum.Cache.UserCache.Mnesia' = Cache) ->
    qlc:q([{UserId, User} || {_Tag, UserId, User} <- Cache:query_handle(),
                              UserId =:= RequestedUserId]);

get(RequestedUserId, Cache) ->
    qlc:q([{UserId, User} || {UserId, User} <- Cache:query_handle(),
                              UserId =:= RequestedUserId]).
