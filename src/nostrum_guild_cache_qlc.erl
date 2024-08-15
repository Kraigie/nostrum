-module(nostrum_guild_cache_qlc).
-export([all/1, get/2]).

-include_lib("stdlib/include/qlc.hrl").

% Optimized channel cache QLC queries.

all(Cache) ->
    qlc:q([Guild || {_GuildId, Guild} <- Cache:query_handle()]).


get(RequestedGuildId, Cache) ->
    QH = case erlang:function_exported(Cache, query_handle, 1) of
             true -> Cache:query_handle([{'==', '$1', {const, RequestedGuildId}}]);
             false -> Cache:query_handle()
         end,
    qlc:q([Guild || {GuildId, Guild} <- QH,
                    GuildId =:= RequestedGuildId]).
