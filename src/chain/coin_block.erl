-module(coin_block).
-author("pfav").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("coin/include/coin.hrl").

%% API
-export([
    genesis/0,
    next/1,
    hash/1,
    hash_it/1,
    is_valid/2,

    lookup/1,
    lookup_by_index/2,
    insert/1,
    prev/1,
    member/1,

    mine/1
]).

%% DEBUG
-export([
    genesis/1,
    i/1,
    text/2
]).

-type hash() :: <<_:512>>.
-type block() :: #block{}.

-export_type([ hash/0, block/0 ]).

%% genesis/0 ------------------------------------------------------------------

-spec genesis() ->
    block().

genesis() ->
    Addr = <<"9A7C41EAC093D44B8B7A2156A605005B79E9DB45E9929ECE972B9358DF2A760B">>,
    PubKey = binary:decode_hex(Addr),
    genesis(PubKey).

genesis(PubKey) ->
    Tx = coin_tx:coinbase(PubKey, 0),
    mine(#block{
        version = 1,
        hash = undefined,
        index = 0,
        prev_hash = <<0:512>>,
        timestamp = 1640489109925,
        txs = [Tx],
        nonce = 0,
        difficulty = 1
    }).

%% next/1 -------------------------------------------------------------------------------

-spec next(PrevBlock) -> NextBlock when
    PrevBlock :: block(),
    NextBlock :: block().

next(#block{version = Vsn, index = Idx, hash = Hash, difficulty = Diff}) ->
    #block{version = Vsn, index = Idx + 1, prev_hash = Hash, timestamp = timestamp(), difficulty = Diff, nonce = 0}.

%% hash/1 ---------------------------------------------------------------------

-spec hash(Block) -> Hash when
    Block :: block(),
    Hash :: hash().

hash(#block{version = Vsn, index = Idx, prev_hash = PrevHash, timestamp = Ts, txs = TXs, difficulty = Dif, nonce = Nonce}) ->
    crypto:hash(blake2b, [
        binary:encode_unsigned(Vsn),
        binary:encode_unsigned(Idx),
        PrevHash,
        binary:encode_unsigned(Ts),
        [H || #tx{hash = H} <- TXs], % TODO: merkel-tree
        binary:encode_unsigned(Dif),
        binary:encode_unsigned(Nonce)
    ]).


%% hash_it/1 ------------------------------------------------------------------

-spec hash_it(Block0) -> Block1 when
    Block0 :: block(),
    Block1 :: block().

hash_it(B) when is_record(B, 'block') ->
    B#block{hash = hash(B)}.


%% is_valid/2 -----------------------------------------------------------------

-spec is_valid(Prev, Cur) -> Result when
    Prev :: block() | undefined,
    Cur :: block(),
    Result :: true | {false, term()}.

is_valid(Prev, Cur)->
    is_valid([
        {is_valid_index, fun is_valid_index/2},
        {is_valid_prev_hash, fun is_valid_prev_hash/2},
        {is_valid_hash, fun is_valid_hash/2},
        {is_hash_correct, fun is_hash_correct/2},
        {is_valid_timestamp, fun is_valid_timestamp/2},
        {is_valid_transaction, fun is_valid_transaction/2}
    ], Prev, Cur).

is_valid([], _, _) ->
    true;
is_valid([{FnName, Fn} | R], Prev, Cur) ->
    case Fn(Prev, Cur) of
        true ->
            is_valid(R, Prev, Cur);
        false ->
            {false, [FnName]};
        {false, Reason} when is_list(Reason) ->
            {false, [FnName | Reason]};
        {false, Term} ->
            {false, [FnName, Term]}
    end.

%% is_valid_index/2 -----------------------------------------------------------

% current-block `index` should be 1-more than prev-block
is_valid_index(#block{index = PrevIdx}, #block{index = CurIdx}) -> PrevIdx + 1 == CurIdx;
% genesis block has no prev and index is zero
is_valid_index(undefined, #block{index = 0}) -> true;
% not valid
is_valid_index(_, _) -> false.

%% is_valid_prev_hash/2 -------------------------------------------------------

% current-block `prev_hash` should be equal to prev-block `hash`
is_valid_prev_hash(#block{hash = Hash}, #block{prev_hash = Hash}) -> true;
% genesis block has no prev and index is zero
is_valid_prev_hash(undefined, #block{index = 0}) -> true;
% not valid
is_valid_prev_hash(_,_) -> false.

%% is_valid_hash/2 ------------------------------------------------------------

% current-block `hash` should comply with proof-of-work
is_valid_hash(_, #block{hash = Hash, difficulty = Diff}) ->
    match_difficulty(Hash, Diff).

%% is_hash_correct/2 ----------------------------------------------------------

% current-block `hash` should be consistent with data
is_hash_correct(_, #block{hash = Hash} = Cur) ->
    Hash == hash(Cur).

%% is_valid_timestamp/2 -------------------------------------------------------

% current-block `timestamp` must at most be 60 seconds in the future
% current-block `timestamp` must at most be 60 seconds in the past relative to prev-block
% genesis block is ok
is_valid_timestamp(_, #block{index = 0}) ->
    true;
is_valid_timestamp(#block{timestamp = T0}, #block{timestamp = T1}) ->
    (T0 - 60000) < T1 andalso (T1 - 60000) < timestamp().

%% is_valid_transaction/2 -----------------------------------------------------

is_valid_transaction(_, #block{txs = []}) ->
    false;
is_valid_transaction(_, #block{prev_hash = PrevHash, index = Index, txs = Txs}) ->
    coin_tx:is_valid(Txs, PrevHash, Index).


%% STORAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lookup/1 -------------------------------------------------------------------

-spec lookup(Hash)
            -> {ok, Block} | {error, term()}
    when
    Hash :: hash(),
    Block :: block().

lookup(Hash) ->
    case coin_db:lookup(?BLOCK_HASH_KEY(Hash)) of
        {ok, Bin} ->
            case coin_proto:decode_block(Bin) of
                {ok, Block} ->
                    {ok, Block};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% lookup_by_index/1 ----------------------------------------------------------

-spec lookup_by_index(Index, Hash)
            -> {ok, Block} | {error, term()} when
    Index :: non_neg_integer(),
    Hash :: hash(),
    Block :: block().

lookup_by_index(Index, Hash) ->
    lookup_by_index_(Index, lookup(Hash)).

lookup_by_index_(Index, {ok, #block{index = Index} = B}) ->
    {ok, B};
lookup_by_index_(Index, {ok, #block{index = I}}) when I < Index ->
    {error, not_found};
lookup_by_index_(Index, {ok, #block{index = I} = B}) when I > Index ->
    lookup_by_index_(Index, prev(B));
lookup_by_index_(_, {error, Reason}) ->
    {error, Reason}.


%% insert/1 -------------------------------------------------------------------

-spec insert(Block)
            -> ok | {error, term()}
    when
    Block :: block().

insert(Block) ->
    try
        {ok, Bin} = coin_proto:encode_block(Block),
        coin_db:insert(?BLOCK_HASH_KEY(Block#block.hash), Bin)
    catch K:E:S ->
        ?LOG_ERROR("[BLOCK] DB Error ~p ~p ~p", [K, E, S]),
        {error, failed}
    end.

%% prev/1 -------------------------------------------------------------------

-spec prev(Block1)
            -> {ok, Block0} | {error, term()}
    when
    Block1 :: block(),
    Block0 :: block().

prev(#block{prev_hash = Hash}) when is_binary(Hash) ->
    lookup(Hash);
prev(_Block) ->
    {error, not_found}.

%% member/1 -------------------------------------------------------------------

-spec member(Hash :: hash())
            -> boolean().

member(Hash) ->
    case coin_db:lookup(?BLOCK_HASH_KEY(Hash)) of
        {ok, _} ->
            true;
        {error, _} ->
            false
    end.

%% CONSENSUS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% mine/1 ---------------------------------------------------------------------

mine(Block) ->
    mine_(Block, 0).

mine_(Block, Nonce) ->
    Hash = hash(Block#block{nonce = Nonce}),
    case match_difficulty(Hash, Block#block.difficulty) of
        true ->
            Block#block{hash = Hash, nonce = Nonce};
        false ->
            mine_(Block, Nonce + 1)
    end.

match_difficulty(Hash, Difficulty) ->
    case Hash of
        <<0:Difficulty, _/bitstring>> ->
            true;
        _ ->
            false
    end.


%% DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(Block) ->
    text(standard_io, Block).

text(Io, B) when is_record(B, 'block') ->
    io:format(Io, "== block#~w~n",       [ B#block.index ]),
    io:format(Io, "version:    ~w~n",    [ B#block.version]),
    io:format(Io, "hash:       ~s~n",    [ binary:encode_hex(B#block.hash) ]),
    io:format(Io, "prev_hash:  ~s~n",    [ binary:encode_hex(B#block.prev_hash) ]),
    io:format(Io, "timestamp:  ~w ~s~n", [ B#block.timestamp, calendar:system_time_to_rfc3339(B#block.timestamp, [{unit, millisecond}]) ]),
    io:format(Io, "nonce:      ~w~n",    [ B#block.difficulty ]),
    io:format(Io, "difficulty: ~w~n",    [ B#block.nonce ]),
    text_transaction(Io, 0, B#block.txs);
text(Io, What) ->
    error(badarg, [Io, What]).

text_transaction(_, _, []) ->
    ok;
text_transaction(Io, N, [Tx | Txs]) ->
    io:format("== tx#~w~n", [N]),
    coin_tx:text(Io, Tx),
    text_transaction(Io, N+1, Txs).


%% UTIL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    erlang:system_time(millisecond).

-ifdef(TEST).

is_valid_test() ->
    ?assert(is_valid(undefined, genesis())),
    ok.
-endif.
