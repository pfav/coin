-module(coin_db).
-author("pfav").

-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

%% API
-export([
    init/0,

    open/1,
    insert/2,
    lookup/1,
    each/2,
    each_keys/2,
    fold/3,
    fold_keys/3,

    delete/1,

    clear/0,
    clear/1,

    count/0,
    count/1,

    dump/0,

    close/0,

    set_handle/1,
    get_handle/0
]).


opts() ->
    [
        {create_if_missing, true},
        {use_bloomfilter, true},
        {compression, true}
    ].

init() ->
    {ok, DbPath} = coin:env([chain, db, path]),
    open(DbPath).

open(Dir) when is_binary(Dir)->
    open(binary_to_list(Dir));
open(Dir) when is_list(Dir) ->
    case eleveldb:open(Dir, opts()) of
        {ok, Handle} ->
            ?LOG_INFO("[DB] opened succesfully ~s", [Dir]),
            set_handle(Handle);
        {error, Reason} ->
            ?LOG_CRITICAL("[DB] failed opening ~s", [Dir]),
            {error, Reason}
    end.

insert(Key, Value) ->
    eleveldb:put(get_handle(), Key, Value, [{sync, true}]).

lookup(Key) ->
    case eleveldb:get(get_handle(), Key, [{fill_cache, true}]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason};
        not_found ->
            {error, not_found}
    end.

delete(Key) ->
    eleveldb:delete(get_handle(), Key, []).

-define(MATCH_PREFIX(K, P), (binary_part((K), {0, size((P))}) == (P))).
-define(STRIP_PREFIX(K, P), (binary_part((K), {size((P)), size((K))-size((P))}))).

% fold/3 ----------------------------------------------------------------------

-spec fold(Fun, Acc0, Prefix) -> Acc1 when
    Span :: binary(),
    Fun :: fun((Span, Span) -> term()),
    Acc0 :: term(),
    Acc1 :: term(),
    Prefix :: binary().

fold(Fun, Acc0, Prefix) ->
    {ok, Itr} = eleveldb:iterator(get_handle(), []),
    do_fold(Itr, Fun, Prefix, Acc0).

fold_keys(Fun, Prefix, Acc0) ->
    {ok, Itr} = eleveldb:iterator(get_handle(), [], keys_only),
    do_fold(Itr, Fun, Prefix, Acc0).

do_fold(Itr, Fun, Prefix, Acc0) ->
    try
        fold_loop(eleveldb:iterator_move(Itr, Prefix), Itr, Fun, Prefix, Acc0)
    of
        L ->
            L
    after
        eleveldb:iterator_close(Itr)
    end.

fold_loop({ok, K}, Itr, Fun, Prefix, Acc0) when ?MATCH_PREFIX(K, Prefix) ->
    Acc1 = Fun(?STRIP_PREFIX(K, Prefix), Acc0),
    fold_loop(eleveldb:iterator_move(Itr, prefetch), Itr, Fun, Prefix, Acc1);
fold_loop({ok, K, V}, Itr, Fun, Prefix, Acc0) when ?MATCH_PREFIX(K, Prefix) ->
    Acc1 = Fun({?STRIP_PREFIX(K, Prefix), V}, Acc0),
    fold_loop(eleveldb:iterator_move(Itr, prefetch), Itr, Fun, Prefix, Acc1);
fold_loop({error, iterator_closed}, _Itr, _Fun, _Prefix, Acc0) ->
    Acc0;
fold_loop({error, invalid_iterator}, _Itr, _Fun, _Prefix, Acc0) ->
    Acc0;
fold_loop(_Result, _Itr, _Fun, _Prefix, Acc) ->
    Acc.

% each -------

each(Fun, Prefix) ->
    {ok, Itr} = eleveldb:iterator(get_handle(), []),
    do_each(Itr, Fun, Prefix).
each_keys(Fun, Prefix) ->
    {ok, Itr} = eleveldb:iterator(get_handle(), [], keys_only),
    do_each(Itr, Fun, Prefix).

do_each(Itr, Fun, Prefix) ->
    try
        each_loop(eleveldb:iterator_move(Itr, Prefix), Itr, Fun, Prefix)
    after
        eleveldb:iterator_close(Itr)
    end.

each_loop({ok, K}, Itr, Fun, Prefix) when ?MATCH_PREFIX(K, Prefix) ->
    case Fun(?STRIP_PREFIX(K, Prefix)) of
        stop ->
            ok;
        {stop, R} ->
            {ok, R};
        _ ->
            each_loop(eleveldb:iterator_move(Itr, prefetch), Itr, Fun, Prefix)
    end;
each_loop({ok, K, V}, Itr, Fun, Prefix) when ?MATCH_PREFIX(K, Prefix) ->
    case Fun({?STRIP_PREFIX(K, Prefix), V}) of
        stop ->
            ok;
        {stop, R} ->
            {ok, R};
        _ ->
            each_loop(eleveldb:iterator_move(Itr, prefetch), Itr, Fun, Prefix)
    end;
each_loop({error, iterator_closed}, _Itr, _Fun, _Prefix) ->
    ok;
each_loop({error, invalid_iterator}, _Itr, _Fun, _Prefix) ->
    ok;
each_loop(_Result, _Itr, _Fun, _Prefix) ->
    ok.

clear() ->
    clear(<<>>).
clear(Prefix) ->
    each_keys(fun (K) -> delete(K) end, Prefix).

count() ->
    count(<<>>).
count(Prefix) ->
    fold_keys(fun (_, Acc) -> Acc + 1 end, Prefix, 0).

dump() ->
    each(fun ({K, V}) ->
       io:format("~s:~s~n", [binary:encode_hex(K), binary:encode_hex(V)])
         end, <<>>).

close() ->
    case eleveldb:close(get_handle()) of
        ok ->
            set_handle(undefined);
        {error, Reason} ->
            {error, Reason}
    end.


get_handle() ->
    persistent_term:get(?MODULE).
set_handle(Db) ->
    persistent_term:put(?MODULE, Db).
