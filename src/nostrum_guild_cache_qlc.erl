-module(nostrum_guild_cache_qlc).
-export([all/1, get/2]).

-include_lib("stdlib/include/qlc.hrl").

% Optimized channel cache QLC queries.

all(Cache) ->
    qlc:q([Guild || {_GuildId, Guild} <- Cache:query_handle()]).


get(RequestedGuildId, Cache) ->
    qlc:q([Guild || {GuildId, Guild} <- Cache:query_handle(),
                    GuildId =:= RequestedGuildId]).
