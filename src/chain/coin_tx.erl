-module(coin_tx).
-author("pfav").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("coin/include/coin.hrl").

%% API
-export([
    new/0,
    add_input/4,
    add_output/3,
    hash/1,
    hash_it/1,
    is_valid/3,
    coinbase/2
]).

%% STORAGE
-export([
    lookup/2,
    lookup_txo/3,
    is_spent/3,
    utxo/2,
    balance/2
]).

%% DEBUG
-export([
    i/1,
    text/2
]).


-type tx()   :: #tx{}.
-type out()  :: #tx_out{}.
-type in()   :: #tx_in{}.
-type hash() :: <<_:512>>.
-type unspent_txo() :: #{ { hash(), non_neg_integer() } => non_neg_integer() }.

-export_type([ tx/0, out/0, in/0, hash/0, unspent_txo/0 ]).


%% new/0 ----------------------------------------------------------------------

-spec new() ->
    tx().

new() ->
    #tx{}.

%% add_input/4 ----------------------------------------------------------------

-spec add_input(PrivKey, Hash, Idx, Tx0) -> Tx1 when
    PrivKey :: coin_key:private_key(),
    Hash :: hash(),
    Idx :: non_neg_integer(),
    Tx0 :: tx(),
    Tx1 :: tx().

add_input(PrivKey, Hash, Idx, Tx0 = #tx{in = In0}) ->
    TxIn0 = #tx_in{out_hash = Hash, out_index = Idx},
    Sign = coin_key:sign(sign_serialize(TxIn0), PrivKey),
    TxIn1 = TxIn0#tx_in{signature = Sign},

    In1 = In0 ++ [TxIn1],
    Tx1 = Tx0#tx{in = In1},
    Tx1.

%% add_output/3 ---------------------------------------------------------------

-spec add_output(PubKey, Amount, Tx0) -> Tx1 when
    PubKey :: coin_key:public_key(),
    Amount :: non_neg_integer(),
    Tx0 :: tx(),
    Tx1 :: tx().

add_output(PubKey, Amount, Tx = #tx{out = Out}) ->
    TxOut = #tx_out{address = PubKey, amount = Amount},
    Tx#tx{out = Out ++ [TxOut]}.

%% hash/1 ---------------------------------------------------------------------

-spec hash(Tx) -> Hash when
    Tx :: tx(),
    Hash :: hash().

hash(#tx{in = TxIn, out = TxOut}) ->
    crypto:hash(blake2b, [
        lists:map(fun hash_serialize/1, TxIn),
        lists:map(fun hash_serialize/1, TxOut)
    ]).

%% hash_it/1 ------------------------------------------------------------------

-spec hash_it(Tx0) -> Tx1 when
    Tx0 :: tx(),
    Tx1 :: tx().

hash_it(Tx) when is_record(Tx, 'tx') ->
    Tx#tx{hash = hash(Tx)}.

%% is_valid/2 -----------------------------------------------------------------

-spec is_valid(Txs, ChainTopHash, BlockIndex) -> Result when
    Txs :: [tx()],
    ChainTopHash :: coin_block:hash(),
    BlockIndex :: non_neg_integer(),
    Result :: true | {false, term()}.

is_valid(Txs, ChainTopHash, BlockIndex) when is_list(Txs) ->
    is_valid_coinbase_head(split_if_coinbase(Txs, BlockIndex), ChainTopHash, BlockIndex).

is_valid_coinbase_head({undefined, Txs}, ChainTopHash, BlockIndex) ->
    is_valid_transaction_list(Txs, ChainTopHash, BlockIndex);
is_valid_coinbase_head({Coinbase, Txs}, ChainTopHash, BlockIndex) ->
    case is_valid_coinbase_tx(Coinbase, BlockIndex) of
        true ->
            is_valid_transaction_list(Txs, ChainTopHash, BlockIndex);
        false ->
            {false, is_valid_coinbase_tx}
    end.

is_valid_transaction_list(Txs, ChainTopHash, BlockIndex) ->
    is_valid([
        {is_txs_valid_size, fun is_txs_valid_size/3},
        {is_txs_hash_unique, fun is_txs_hash_unique/3},
        {is_txs_input_unique, fun is_txs_input_unique/3},
        {is_valid_tx_each, fun is_valid_tx_each/3}
    ], Txs, ChainTopHash, BlockIndex).

%% ----------------------------------------------------------------------------

%% txs range [2, 128] (once added coinbase)
%% genesis only one allowed to have one coinbase-transaction
is_txs_valid_size(_, _ChainTopHash, 0) ->
    true;
is_txs_valid_size(Txs, _ChainTopHash, _BlockIndex) ->
    Len = length(Txs),
    Len >= 1 andalso Len < ?MAX_TX_PER_BLOCK.

%% ----------------------------------------------------------------------------

%% hash must be unique in txs

is_txs_hash_unique(Txs, _ChainTopHash, _BlockIndex) ->
    Hs = [ Hash || #tx{hash = Hash} <- Txs],
    length(lists:usort(Hs)) == length(Hs).

%% ----------------------------------------------------------------------------

% tx_in must be unique over the transactions

is_txs_input_unique(Txs, _ChainTopHash, _BlockIndex) ->
    is_input_unique_tx(Txs, #{}).

is_input_unique_tx([], _Acc) ->
    true;
is_input_unique_tx([#tx{in = In} | R], Acc0) ->
    case is_input_unique_in(In, Acc0) of
        {continue, Acc1} ->
            is_input_unique_tx(R, Acc1);
        error ->
            false
    end.

is_input_unique_in([], Acc) ->
    {continue, Acc};
is_input_unique_in([#tx_in{out_hash = undefined, signature = undefined} | R], Acc) ->
    is_input_unique_in(R, Acc);
is_input_unique_in([#tx_in{out_hash = Hash, out_index = Idx} | R], Acc) ->
    case maps:is_key({Hash, Idx}, Acc) of
        true ->
            error;
        false ->
            is_input_unique_in(R, Acc#{ {Hash, Idx} => true })
    end.

%% ----------------------------------------------------------------------------

% transaction validator iterator

is_valid_tx_each([], _ChainTopHash, _BlockIndex) ->
    true;
is_valid_tx_each([Tx | R], ChainTopHash, BlockIndex) ->
    case is_valid_tx(Tx, ChainTopHash, BlockIndex) of
        true ->
            is_valid_tx_each(R, ChainTopHash, BlockIndex);
        Res ->
            Res
    end.

is_valid_tx(Tx = #tx{}, ChainTopHash, BlockIndex) ->
    is_valid([
        {is_valid_hash, fun is_valid_hash/3},
        {is_valid_input, fun is_valid_input/3},
        {is_valid_not_double_spent, fun is_valid_not_double_spent/3},
        {is_valid_amount, fun is_valid_amount/3}
        % TODO: tx_out.amount != 0
    ], Tx, ChainTopHash, BlockIndex);
is_valid_tx(_, _, _) ->
    {false, badarg}.

is_valid([], _ChainTopHash, _BlockIndex, _Tx) ->
    true;
is_valid([{FnName, Fn} | R], Tx, ChainTopHash, BlockIndex) ->
    case Fn(Tx, ChainTopHash, BlockIndex) of
        true ->
            is_valid(R, Tx, ChainTopHash, BlockIndex);
        false ->
            {false, [FnName]};
        {false, Reason} when is_list(Reason) ->
            {false, [FnName | Reason]};
        {false, Reason} ->
            {false, [FnName, Reason]}
    end.

%% is_valid_hash/2 ------------------------------------------------------------

is_valid_hash(#tx{hash = Hash} = Tx, _, _) ->
    Hash == hash(Tx).

%% is_valid_input/2 -----------------------------------------------------------

is_valid_input(#tx{in = Ins}, ChainTopHash, _BlockIndex) ->
    validate_input_(Ins, ChainTopHash).

validate_input_([], _ChainTopHash) ->
    true;
validate_input_([#tx_in{signature = undefined} | _], _ChainTopHash) ->
    {false, too_many_coinbase};
validate_input_([TxIn | R], ChainTopHash) ->
    case lookup_txo(ChainTopHash, TxIn#tx_in.out_hash, TxIn#tx_in.out_index) of
        {ok, TxOut} ->
            case coin_key:verify(sign_serialize(TxIn), TxIn#tx_in.signature, TxOut#tx_out.address) of
                true ->
                    validate_input_(R, ChainTopHash);
                false ->
                    false
            end;
        {error, Reason} ->
            {false, Reason}
    end.

is_valid_not_double_spent(#tx{in = Ins}, ChainTopHash, _BlockIndex) ->
    is_valid_not_double_spent_(Ins, ChainTopHash).

is_valid_not_double_spent_([], _ChainTopHash) ->
    true;
is_valid_not_double_spent_([#tx_in{signature = undefined} | _R], _ChainTopHash) ->
    {false, too_many_coinbase};
is_valid_not_double_spent_([TxIn | R], ChainTopHash) ->
    case is_spent(ChainTopHash, TxIn#tx_in.out_hash, TxIn#tx_in.out_index) of
        {ok, false} ->
            is_valid_not_double_spent_(R, ChainTopHash);
        {ok, true} ->
            false;
        {error, Reason} ->
            {error, Reason}
    end.

%% is_valid_value/2 -----------------------------------------------------------

is_valid_amount(#tx{in = In, out = Out}, ChainTopHash, _BlockIndex) ->
    case amount(In, ChainTopHash) of
        {ok, AmountIn} ->
            case amount(Out, ChainTopHash) of
                {ok, AmountOut} ->
                    AmountIn >= AmountOut;
                {error, Reason} ->
                    {false, Reason}
            end;
        {error, Reason} ->
            {false, Reason}
    end.

amount(L, ChainTopHash) ->
    amount(L, ChainTopHash, 0).

amount([], _ChainTopHash, Acc) ->
    {ok, Acc};
amount([#tx_out{amount = Amount} | R], ChainTopHash, Acc) ->
    amount(R, ChainTopHash, Amount + Acc);
amount([#tx_in{signature = undefined, out_hash = undefined} | R], ChainTopHash, Acc) -> % coinbase
    amount(R, ChainTopHash, Acc + ?COINBASE_AMOUNT); % TODO: get coinbase amount based on block-index
amount([#tx_in{} = TxIn | R], ChainTopHash, Acc) ->
    case lookup_txo(ChainTopHash, TxIn#tx_in.out_hash, TxIn#tx_in.out_index) of
        {ok, TxOut} ->
            amount([TxOut | R], ChainTopHash, Acc);
        {error, Reason} ->
            {error, Reason}
    end.


%% STORAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% lookup/2 -------------------------------------------------------------------

-spec lookup(ChainTopHash, Hash) -> {ok, Tx} | {error, any()} when
    ChainTopHash :: coin_block:hash(),
    Hash :: hash(),
    Tx :: tx().

lookup(<<0:512>>, TxHash) when is_binary(TxHash) ->
    {error, not_found};
lookup(ChainTopHash, TxHash) when is_binary(ChainTopHash), is_binary(TxHash) ->
    case coin_block:lookup(ChainTopHash) of
        {ok, #block{prev_hash = PrevHash, txs = Txs}} ->
            case lists:keyfind(TxHash, #tx.hash, Txs) of
                Tx = #tx{} ->
                    {ok, Tx};
                false ->
                    lookup(PrevHash, TxHash)
            end;
        {error, Reason} ->
            {error, Reason}
    end;
lookup(_ChainTopHash, _Hash) ->
    {error, badarg}.



%% lookup_txo/2 ---------------------------------------------------------------

-spec lookup_txo(ChainTopHash, Hash, Idx) -> {ok, TxOut} | {error, any()} when
    ChainTopHash :: coin_block:hash(),
    Hash :: hash(),
    Idx :: non_neg_integer(),
    TxOut :: out().

lookup_txo(ChainTopHash, Hash, Idx) when Idx >= 0 ->
    case lookup(ChainTopHash, Hash) of
        {ok, #tx{out = Outs}} when Idx < length(Outs) ->
            {ok, lists:nth(Idx + 1, Outs)};
        _ ->
            {error, not_found}
    end;
lookup_txo(_ChainTopHash, _Hash, _Idx) ->
    {error, badarg}.

%% is_spent/3 -----------------------------------------------------------------

-spec is_spent(ChainTopHash, Hash, Idx) -> {ok, IsSpent} | {error, any()} when
    ChainTopHash :: coin_block:hash(),
    Hash :: hash(),
    Idx :: non_neg_integer(),
    IsSpent :: boolean().

is_spent(<<0:512>>, _, _) ->
    {ok, false};
is_spent(ChainTip, TxHash, Index) ->
    case coin_block:lookup(ChainTip) of
        {ok, #block{prev_hash = PrevHash, txs = Txs}} ->
            case exist_tx_in(Txs, TxHash, Index) of
                true ->
                    {ok, true};
                false ->
                    is_spent(PrevHash, TxHash, Index)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

exist_tx_in([], _Hash, _Idx) ->
    false;
exist_tx_in([#tx{in = In} | R], Hash, Idx) ->
    case exist_tx_in(In, Hash, Idx) of
        true ->
            true;
        false ->
            exist_tx_in(R, Hash, Idx)
    end;
exist_tx_in([#tx_in{out_hash = Hash, out_index = Idx} | _], Hash, Idx) ->
    true;
exist_tx_in([_ | R], Hash, Idx) ->
    exist_tx_in(R, Hash, Idx).

%% balance/2 ------------------------------------------------------------------
%% TODO: need to account for fee  (when address == address_of_miner)

-spec balance(ChainTip, PubKey) -> Balance when
    ChainTip :: coin_block:hash(),
    PubKey :: coin_key:public_key(),
    Balance :: non_neg_integer().

balance(ChainTip, PubKey) ->
    Utxo = utxo(ChainTip, PubKey),
    maps:fold(fun calculate_balance/3, 0, Utxo).

calculate_balance(_, Amount, Acc) ->
    Acc + Amount.


%% utxo/2 ---------------------------------------------------------------------

-spec utxo(ChainTip, PubKey) -> Utxo when
    ChainTip :: coin_block:hash(),
    PubKey :: coin_key:public_key(),
    Utxo :: unspent_txo().

utxo(ChainTip, PubKey) ->
    utxo(ChainTip, PubKey, #{}).

utxo(<<0:512>>, _PubKey, Acc) ->
    maps:filter(fun (_, Amount) -> Amount > 0 end, Acc);
utxo(ChainTip, PubKey, Acc0) ->
    case coin_block:lookup(ChainTip) of
        {ok, #block{prev_hash = PrevHash, txs = Txs}} ->
            utxo(PrevHash, PubKey, collect_utxo(Txs, PubKey, Acc0));
        {error, Reason} ->
            {error, Reason}
    end.

collect_utxo([], _PubKey, Acc) ->
    Acc;
collect_utxo([#tx{hash = Hash, in = In, out = Out} | R], PubKey, Acc0) ->
    Acc1 = add_to_balance(Out, Hash, 0, PubKey, Acc0),
    Acc2 = rm_from_balance(In, Acc1),
    collect_utxo(R, PubKey, Acc2).

add_to_balance([], _TxHash, _Idx, _Address, Acc) ->
    Acc;
add_to_balance([#tx_out{address = Address, amount = Amount} | R], TxHash, Idx, Address, Acc) ->
    case maps:is_key({TxHash, Idx}, Acc) of
        true ->
            add_to_balance(R, TxHash, Idx+1, Address, Acc);
        false ->
            add_to_balance(R, TxHash, Idx+1, Address, Acc#{ {TxHash, Idx} => Amount })
    end;
add_to_balance([_ | R], TxHash, Idx, Address, Acc) ->
    add_to_balance(R, TxHash, Idx+1, Address, Acc).

rm_from_balance([], Acc) ->
    Acc;
rm_from_balance([#tx_in{out_hash = undefined, signature = undefined} | R], Acc) ->
    rm_from_balance(R, Acc);
rm_from_balance([#tx_in{out_hash = TxHash, out_index = Idx} | R], Acc) ->
    rm_from_balance(R, Acc#{ {TxHash, Idx} => 0 }).


%% coinbase/2 -----------------------------------------------------------------

-spec coinbase(Addr, BlockIndex) -> Tx when
    Addr :: coin_key:public_key(),
    BlockIndex :: non_neg_integer(),
    Tx :: tx().

coinbase(Addr, BlockIndex) ->
    hash_it(#tx{
        in = [
            #tx_in{
                out_index = BlockIndex
            }
        ],
        out = [
            #tx_out{
                address = Addr,
                amount = ?COINBASE_AMOUNT
            }
        ]
    }).

is_valid_coinbase_tx(#tx{in = [#tx_in{signature = undefined} = In], out = [Out]}, BlockIndex) ->
    In#tx_in.out_index == BlockIndex andalso
        In#tx_in.out_hash == undefined andalso
        Out#tx_out.amount == ?COINBASE_AMOUNT; % TODO: get coinbase amount based on block-index
is_valid_coinbase_tx(_Coinbase, _BlockIndex) ->
    false.


split_if_coinbase([#tx{in = [#tx_in{signature = undefined}]} = Coinbase | R], _BlockIndex) ->
    {Coinbase, R};
split_if_coinbase(L, _) ->
    {undefined, L}.


% hashing serialization
hash_serialize(#tx_in{out_hash = Hash, out_index = Index, signature = Signature})
    when is_binary(Hash), is_binary(Signature) ->
    [Hash, binary:encode_unsigned(Index), Signature];
hash_serialize(#tx_in{out_index = Index}) ->
    [binary:encode_unsigned(Index)];
hash_serialize(#tx_out{address = Addr, amount = Amount}) ->
    [Addr, binary:encode_unsigned(Amount)].

sign_serialize(#tx_in{out_hash = Hash, out_index = Index}) ->
    [Hash, binary:encode_unsigned(Index)].


%% DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(Arg)
    when
    is_record(Arg, 'tx');
    is_record(Arg, 'tx_in');
    is_record(Arg, 'tx_out')
    ->
    text(standard_io, Arg).

text(Io, #tx{} = Tx) ->
    io:format(Io, "hash: ~s~n", [ binary:encode_hex(Tx#tx.hash) ]),
    text_in(Io, 0, Tx#tx.in),
    text_out(Io, 0, Tx#tx.out);
text(Io, #tx_in{} = TxIn) ->
    io:format(Io, "+: ~s~n", [ text_opt_binary(TxIn#tx_in.out_hash) ]),
    io:format(Io, "+: ~w~n", [ TxIn#tx_in.out_index ]),
    io:format(Io, "+: ~s~n", [ text_opt_binary(TxIn#tx_in.signature) ]);
text(Io, #tx_out{} = TxOut) ->
    io:format(Io, "-: ~s~n", [ binary:encode_hex(TxOut#tx_out.address) ]),
    io:format(Io, "-: ~w~n", [ TxOut#tx_out.amount ]);
text(Io, What) ->
    error(badarg, [Io, What]).

text_in(_, _, []) ->
    ok;
text_in(Io, N, [In | R]) ->
    io:format("-- in#~w~n", [N]),
    text(Io, In),
    text_in(Io, N+1, R).

text_out(_, _, []) ->
    ok;
text_out(Io, N, [Out | R]) ->
    io:format("-- out#~w~n", [N]),
    text(Io, Out),
    text_in(Io, N+1, R).

text_opt_binary(undefined) ->
    "undefined";
text_opt_binary(B) when is_binary(B) ->
    binary:encode_hex(B).


-ifdef(TEST).

is_hash_unique_test() ->
    ?assertEqual(true, is_txs_hash_unique([], <<>>, 0)),
    ?assertEqual(true, is_txs_hash_unique([#tx{hash = <<"abcd">>}], <<>>, 0)),
    ?assertEqual(true, is_txs_hash_unique([
        #tx{hash = <<"abc">>},
        #tx{hash = <<"xyz">>}
    ], <<>>, 0)),
    ?assertEqual(false, is_txs_hash_unique([
        #tx{hash = <<"abc">>},
        #tx{hash = <<"abc">>}
    ], <<>>, 0)),
    ?assertEqual(false, is_txs_hash_unique([
        #tx{hash = <<"abc">>},
        #tx{hash = <<"xyz">>}
        #tx{hash = <<"abc">>}
    ], <<>>, 0)),
    ok.

tx_in_unique_test() ->
    ?assertEqual(true, is_txs_input_unique([], <<>>, 0)),
    ?assertEqual(true, is_txs_input_unique([
        #tx{in = [#tx_in{out_index = 0}]},
        #tx{in = [#tx_in{out_index = 0}]}
    ], <<>>, 0)),

    ?assertEqual(true, is_txs_input_unique([
        #tx{in = [#tx_in{out_hash = <<"abc">>, out_index = 0, signature = <<>>}]},
        #tx{in = [#tx_in{out_hash = <<"xyz">>, out_index = 0, signature = <<>>}]}
    ], <<>>, 0)),

    ?assertEqual(false, is_txs_input_unique([
        #tx{in = [#tx_in{out_index = 0}]},
        #tx{in = [#tx_in{out_hash = <<"abc">>, out_index = 0, signature = <<>>}]},
        #tx{in = [#tx_in{out_index = 0}]},
        #tx{in = [#tx_in{out_hash = <<"xyz">>, out_index = 0, signature = <<>>}]}
        #tx{in = [#tx_in{out_index = 0}]},
        #tx{in = [#tx_in{out_hash = <<"abc">>, out_index = 0, signature = <<>>}]}
    ], <<>>, 0)),

    ok.

tx_is_valid_size_test(_) ->
    ?assertEqual(false, is_txs_valid_size([], <<>>, 0)),
    ?assertEqual(false, is_txs_valid_size([], <<>>, 0)),
    ok.

is_valid_hash_test() ->
    Tx = #tx{
        in = [
            #tx_in{out_hash = <<"abcd">>, out_index = 0, signature = <<"hello">>}
        ],
        out = [
            #tx_out{address = <<"abcd">>, amount = 10}
        ]
    },
    Hash = hash(Tx),

    ?assertEqual(true, is_valid_hash(Tx#tx{hash = Hash}, <<>>, 0)),
    ?assertEqual(false, is_valid_hash(Tx#tx{hash = <<"abcd">>}, <<>>, 0)),

    ok.

is_valid_coinbase_test() ->
    ?assertEqual(true, is_valid_coinbase_tx(coinbase(<<"abcd">>, 0), 0)),
    ?assertEqual(false, is_valid_coinbase_tx(coinbase(<<"abcd">>, 0), 1)),
    ok.


-endif.