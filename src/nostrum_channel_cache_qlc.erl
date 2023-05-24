-module(nostrum_channel_cache_qlc).
-export([get/2]).

-include_lib("stdlib/include/qlc.hrl").

% Optimized channel cache QLC queries.

get(RequestedChannelId, Cache) ->
    qlc:q([{ChannelId, Channel} || {ChannelId, Channel} <- Cache:query_handle(),
                                   ChannelId =:= RequestedChannelId]).
