-module(nostrum_presence_cache_qlc).
-export([get/3]).

-include_lib("stdlib/include/qlc.hrl").

% Optimized presence cache QLC queries.

get(RequestedGuildId, RequestedUserId, Cache) ->
    QH = case erlang:function_exported(Cache, query_handle, 1) of
             true ->
                 Guards = [{'==', '$1', {const, RequestedGuildId}},
                           {'==', '$2', {const, RequestedUserId}}],
                 Cache:query_handle(Guards);
             false ->
                 Cache:query_handle()
         end,
    qlc:q([Presence || {{GuildId, UserId}, Presence} <- QH,
                                   GuildId =:= RequestedGuildId,
                                   UserId =:= RequestedUserId]).
