-module(nostrum_presence_cache_qlc).
-export([get/3]).

-include_lib("stdlib/include/qlc.hrl").

% Optimized presence cache QLC queries.

get(RequestedGuildId, RequestedUserId, Cache) ->
    qlc:q([Presence || {{GuildId, UserId}, Presence} <- Cache:query_handle(),
                                   GuildId =:= RequestedGuildId,
                                   UserId =:= RequestedUserId]).
