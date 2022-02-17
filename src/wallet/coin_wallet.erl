-module(coin_wallet).
-author("pfav").

%% API
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-export([
    balance/1,
    balance/2,
    pay/3,

    handle_http/4
]).

-include_lib("coin/include/coin.hrl").


%% balance/1 ------------------------------------------------------------------

-spec balance(ChainTip)
            -> Balance
    when
    ChainTip :: coin_block:hash(),
    Balance :: non_neg_integer().

balance(ChainTip) ->
    Pub = coin_key:pub(),
    balance(ChainTip, Pub).

%% balance/2 ------------------------------------------------------------------

-spec balance(ChainTip, Address)
            -> Balance
    when
    ChainTip :: coin_block:hash(),
    Address :: coin_key:public_key(),
    Balance :: non_neg_integer().

balance(ChainTip, Address) ->
    coin_tx:balance(ChainTip, Address).

%% pay/3 ----------------------------------------------------------------------

-spec pay(ChainTip, ToAddress, Amount)
            -> {ok, Tx} | {error, term()}
    when
    ChainTip :: coin_block:hash(),
    ToAddress :: coin_key:public_key(),
    Amount :: non_neg_integer(),
    Tx :: coin_tx:tx().

pay(ChainTip, ToAddress, Amount) when Amount > 0 ->
    Keypair = coin_key:get_key(),
    Balance = balance(ChainTip, Keypair#keypair.pub),
    if
        Amount > Balance ->
            {error, not_enough_balance};
        true ->
            Utxo = maps:to_list(coin_tx:utxo(ChainTip, Keypair#keypair.pub)),
            make_transaction(Keypair, ToAddress, Amount, 0, coin_tx:new(), Utxo)
    end;
pay(_ChainTip, _ToAddress, _Amount) ->
    {error, invalid_amount}.

make_transaction(Keypair, ToAddr, Amount, Sum, Tx0, _)
    when Sum >= Amount ->
    Tx1 = coin_tx:add_output(ToAddr, Amount, Tx0),
    maybe_add_change(Keypair, Amount, Sum, Tx1);
make_transaction(Keypair, ToAddr, Amount, Sum, Tx0, [{{Hash, Idx}, Am } | R]) ->
    Tx1 = coin_tx:add_input(Keypair#keypair.priv, Hash, Idx, Tx0),
    make_transaction(Keypair, ToAddr, Amount, Sum + Am, Tx1, R).

maybe_add_change(Keypair, Amount, Sum, Tx0) ->
    Change = Sum - Amount,
    if
        Change > 0 ->
            Tx1 = coin_tx:add_output(Keypair#keypair.pub, Change, Tx0),
            {ok, coin_tx:hash_it(Tx1)};
        true ->
            {ok, coin_tx:hash_it(Tx0)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HTTP SERVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_http(<<"GET">>, <<"/api/wallet/:hash">>, _Req, Data) ->
    ChainTip = maps:get(hash, Data),
    Keypair = coin_key:get_key(),
    W0 = coin_key:to_json(Keypair),
    W1 = W0#{balance => balance(ChainTip, Keypair#keypair.pub)},
    W2 = W1#{keyfile => coin_key:filename()},
    {200, coin_http:headers(), W2};

handle_http(<<"POST">>, <<"/api/wallet/:hash/pay">>, _Req, Data) ->
    ChainTip = maps:get(hash, Data),
    ToAddr = binary:decode_hex(maps:get(address, Data)),
    Amount = maps:get(amount, Data),
    case pay(ChainTip, ToAddr, Amount) of
        {ok, Tx} ->
            % TODO: submit to mempool and have miner process every once in a while check
            %       to create new block
            %ok = coin_mempool:submit(Tx),

            % NOTE: mining of the block ill be removed. it's just for testing
            {ok, LatestBlock} = coin_block:lookup(ChainTip),
            NewBlock = coin_block:next(LatestBlock),
            Coinbase = coin_tx:coinbase(coin_key:pub(), NewBlock#block.index),
            MinedBlock = coin_block:mine(NewBlock#block{txs = [Coinbase, Tx]}),
            case coin_chain:append(MinedBlock) of
                ok ->
                    {200, coin_http:headers(), #{result => <<"ok">>}};
                {error, Reason} ->
                    ?LOG_WARNING("[WALLET] failed to add block to address ~s amount ~w reason ~p", [ToAddr, Amount, Reason]),
                    {200, coin_http:headers(), #{result => <<"error">>, message => <<"failed to add block">>}}
            end;
        {error, Reason} when is_atom(Reason) ->
            ?LOG_WARNING("[WALLET] failed to create transaction to ~s amount ~w reason ~p", [ToAddr, Amount, Reason]),
            {200, coin_http:headers(), #{result => <<"error">>, message => Reason}}
%%        {error, Reason} ->
%%            ?LOG_WARNING("[WALLET] failed to create transaction to ~s amount ~w reason ~p", [ToAddr, Amount, Reason]),
%%            {200, coin_http:headers(), #{result => <<"error">>, message => <<"failed">>}}
    end;

handle_http(<<"GET">>, <<"/api/wallet/:hash/utxo">>, Req, Data) ->
    Qs = cowboy_req:match_qs([
        {address, [fun coin_http:constraint_valid_wallet_address/2]}
    ], Req),
    ChainTip = maps:get(hash, Data),
    Address = maps:get(address, Qs),
    UnspentTXO0 = coin_tx:utxo(ChainTip, Address),
    UnspentTXO1 = maps:fold(
        fun ({TxHash, TxOutIdx}, Amount, Acc) ->
            erlang:display(TxHash),
            [
                #{
                'out-tx-hash' => binary:encode_hex(TxHash),
                'out-tx-out-index' => TxOutIdx,
                amount => Amount
            } | Acc]
        end, [], UnspentTXO0),
    {200, coin_http:headers(), UnspentTXO1}.