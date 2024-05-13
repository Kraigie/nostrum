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

-module(nostrum_message_cache_qlc).
-export([by_channel/2, by_channel_and_author/3, by_author/2, by_author/4, sorted_by_age/1]).

-include_lib("stdlib/include/qlc.hrl").

% The matching on the cache names here is smelly. But we need it for the
% built-in caches for now.

-define(MNESIA_CACHE, 'Elixir.Nostrum.Cache.MessageCache.Mnesia').

% These must be selected carefully so that QLC can plan using the indices properly.
-define(MNESIA_FORMAT, {_Tag, MessageId, ChannelId, AuthorId, Message}).

% The returned query handle is sorted by message id, however, due to
% limitations of QLC, this means the result is a list of tuples of the form
% {MessageId, Message}.
-spec by_channel('Elixir.Nostrum.Struct.Channel':id(), module()) -> qlc:query_handle().
by_channel(RequestedChannelId, ?MNESIA_CACHE) ->
    Q1 = qlc:q([{MessageId, Message} || {_Tag, MessageId, ChannelId, _, Message} <- ?MNESIA_CACHE:query_handle(),
                                ChannelId =:= RequestedChannelId]),
    qlc:keysort(1, Q1);

by_channel(RequestedChannelId, Cache) ->
    Q1 = qlc:q([{MessageId, Message} || {MessageId, Message} <- Cache:query_handle(),
                                ChannelId = map_get(channel_id, Message),
                                ChannelId =:= RequestedChannelId]),
    qlc:keysort(1, Q1).

% Lookup all cached messages in a channel by a specific user.
% The output is not sorted.
-spec by_channel_and_author('Elixir.Nostrum.Struct.Channel':id(), 'Elixir.Nostrum.Struct.Message':id(), module()) -> qlc:query_handle().
by_channel_and_author(RequestedChannelId, RequestedUserId, ?MNESIA_CACHE) ->
    qlc:q([Message || {_Tag, {_ChannelId, _MessageId}, ChannelId, AuthorId, Message} <- ?MNESIA_CACHE:query_handle(),
                                ChannelId =:= RequestedChannelId,
                                AuthorId =:= RequestedUserId]);

by_channel_and_author(RequestedChannelId, RequestedUserId, Cache) ->
    qlc:q([Message || {{ChannelId, AuthorId}, Message} <- Cache:query_handle(),
                                ChannelId =:= RequestedChannelId,
                                AuthorId =:= RequestedUserId]).

% Lookup all cached messages by a specific user.
% The output is sorted by message id.
-spec by_author('Elixir.Nostrum.Struct.User':id(), module()) -> qlc:query_handle().
by_author(RequestedUserId, ?MNESIA_CACHE) ->
    Q1 = qlc:q([{MessageId, Message} || {_Tag, MessageId, _ChannelId, AuthorId, Message} <- ?MNESIA_CACHE:query_handle(),
                                AuthorId =:= RequestedUserId]),
    qlc:keysort(1, Q1);

by_author(RequestedUserId, Cache) ->
    Q1 = qlc:q([{MessageId, Message} || {MessageId, Message} <- Cache:query_handle(),
                                Author = map_get(author, Message),
                                AuthorId = map_get(id, Author),
                                AuthorId =:= RequestedUserId]),
    qlc:keysort(1, Q1).

% Lookup all cached messages by a specific user.
% with a message id greater than After and less than Before.
-spec by_author('Elixir.Nostrum.Struct.User':id(), Before :: non_neg_integer(), After :: non_neg_integer(), module()) -> qlc:query_handle().
by_author(RequestedUserId, Before, After, ?MNESIA_CACHE) ->
    Q1 = qlc:q([{MessageId, Message} || {_Tag, MessageId, _ChannelId, AuthorId, Message} <- ?MNESIA_CACHE:query_handle(),
                                AuthorId =:= RequestedUserId,
                                MessageId =< Before,
                                MessageId >= After]),
    qlc:keysort(1, Q1);

by_author(RequestedUserId, Before, After, Cache) ->
    Q1 = qlc:q([{MessageId, Message} || {MessageId, Message} <- Cache:query_handle(),
                                Author = map_get(author, Message),
                                AuthorId = map_get(id, Author),
                                AuthorId =:= RequestedUserId,
                                MessageId =< Before,
                                MessageId >= After]),
    qlc:keysort(1, Q1).

% Lookup the id of cached messages sorted by message id.
-spec sorted_by_age(module()) -> qlc:query_handle().
sorted_by_age(?MNESIA_CACHE) ->
    Q1 = qlc:q([ MessageId || {_Tag, MessageId, _ChannelId, _AuthorId, _Message} <- ?MNESIA_CACHE:query_handle()]),
    qlc:sort(Q1);

sorted_by_age(Cache) ->
    Q1 = qlc:q([MessageId || {MessageId, _Message} <- Cache:query_handle()]),
    qlc:sort(Q1).
