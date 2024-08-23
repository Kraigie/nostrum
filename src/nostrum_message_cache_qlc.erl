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
%
% NOTE: None of the functions in this module make any guarantees about the
% sorting of the output. If you need a specific order, you must sort the output
% yourself either by using `qlc:keysort` or by using `lists:sort` on the output.

-module(nostrum_message_cache_qlc).
-export([
    by_channel/4,
    by_channel_and_author/5,
    by_author/4,
    all_message_ids_in_channel/2
]).

-include_lib("stdlib/include/qlc.hrl").

% The matching on the cache names here is smelly. But we need it for the
% built-in caches for now.

% These must be selected carefully so that QLC can plan using the indices properly.
-define(MNESIA_FORMAT, {_Tag, MessageId, ChannelId, AuthorId, Message}).

% Fetch all messages in a channel before a given message id,
% and after another given message id. Accepts infinity as a before
% value since erlang term ordering makes atoms always larger than integers.
-spec by_channel(
    'Elixir.Nostrum.Struct.Channel':id(), non_neg_integer() | infinity, non_neg_integer(), qlc:query_handle()
) -> qlc:query_handle().
by_channel(RequestedChannelId, After, Before, Handle) ->
    qlc:q([
        Message
     || {_Tag, MessageId, ChannelId, _, Message} <- Handle,
        ChannelId =:= RequestedChannelId,
        MessageId =< Before,
        MessageId >= After
    ]).

% lookup the IDs of all cached messages for a given channel.
-spec all_message_ids_in_channel('Elixir.Nostrum.Struct.Channel':id(), qlc:query_handle()) ->
    qlc:query_handle().
all_message_ids_in_channel(RequestedChannelId, Handle) ->
    qlc:q([
        MessageId
     || {_Tag, MessageId, ChannelId, _, _Message} <- Handle,
        ChannelId =:= RequestedChannelId
    ]).

% Lookup all cached messages in a channel by a specific user.
% The output is not sorted.
-spec by_channel_and_author(
    'Elixir.Nostrum.Struct.Channel':id(), 'Elixir.Nostrum.Struct.Message':id(), non_neg_integer(), non_neg_integer() | infinity, qlc:query_handle()
) -> qlc:query_handle().
by_channel_and_author(RequestedChannelId, RequestedUserId, After, Before, Handle) ->
    qlc:q([
        Message
     || {_Tag, MessageId, ChannelId, AuthorId, Message} <- Handle,
        ChannelId =:= RequestedChannelId,
        AuthorId =:= RequestedUserId,
        MessageId =< Before,
        MessageId >= After
    ]).

% Lookup all cached messages by a specific user.
% with a message id greater than After and less than Before.
-spec by_author(
    'Elixir.Nostrum.Struct.User':id(),
    After :: non_neg_integer(),
    Before :: non_neg_integer(),
    qlc:query_handle()
) -> qlc:query_handle().
by_author(RequestedUserId, After, Before, Handle) ->
    qlc:q([
        Message
     || {_Tag, MessageId, _ChannelId, AuthorId, Message} <- Handle,
        AuthorId =:= RequestedUserId,
        MessageId =< Before,
        MessageId >= After
    ]).

