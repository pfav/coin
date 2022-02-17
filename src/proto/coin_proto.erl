-module(coin_proto).
-author("pfav").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("coin/include/coin.hrl").
-include("./CoinProto.hrl").


%% API
-export([
    %% branch
    encode_branch/1,
    decode_branch/1,
    encode_branch_json/1,
    decode_branch_json/1,
    encode_branchset_json/1,
    decode_branchset_json/1,

    %% block
    encode_block/1,
    decode_block/1,
    encode_block_json/1,
    decode_block_json/1
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Branch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% encode_branch/1 ------------------------------------------------------------

-spec encode_branch(Branch :: #branch{}) ->
    {ok, binary()} | {error, term()}.

encode_branch(#branch{} = B) ->
    'CoinProto':encode('Branch', wrap_branch(B));
encode_branch(B) ->
    error(badarg, [B]).

%% decode_branch/1 ------------------------------------------------------------

-spec decode_branch(Bin :: binary()) ->
    {ok, #branch{}} | {error, term()}.

decode_branch(Bin) when is_binary(Bin) ->
    case 'CoinProto':decode('Branch', Bin) of
        {ok, Branch} ->
            {ok, unwrap_branch(Branch)};
        {error, Reason} ->
            {error, Reason}
    end;
decode_branch(Bin) ->
    error(badarg, [Bin]).

%% encode_branch_json/1 -------------------------------------------------------

-spec encode_branch_json(Branch :: #branch{}) ->
    {ok, binary()} | {error, term()}.

encode_branch_json(#branch{} = B) ->
    'CoinProto':jer_encode('Branch', wrap_branch(B));
encode_branch_json(B) ->
    error(badarg, [B]).

%% decode_branch_json/1 -------------------------------------------------------

-spec decode_branch_json(Bin :: binary()) ->
    {ok, #branch{}} | {error, term()}.

decode_branch_json(Bin) when is_binary(Bin) ->
    case 'CoinProto':jer_decode('Branch', Bin) of
        {ok, Branch} ->
            {ok, unwrap_branch(Branch)};
        {error, Reason} ->
            {error, Reason}
    end;
decode_branch_json(Bin) ->
    error(badarg, [Bin]).

%% encode_branch_set_json/1 ---------------------------------------------------

-spec encode_branchset_json([ Branch :: #branch{} ]) ->
    {ok, binary()} | {error, term()}.

encode_branchset_json(BranchSet) when is_list(BranchSet) ->
    'CoinProto':jer_encode('BranchSet', branch_set_enc(BranchSet));
encode_branchset_json(L) ->
    error(badarg, [L]).

%% decode_branch_set_json/1 ---------------------------------------------------

-spec decode_branchset_json(Bin :: binary()) ->
    {ok, [#branch{}]} | {error, term()}.

decode_branchset_json(Bin) when is_binary(Bin) ->
    case 'CoinProto':jer_decode('BranchSet', Bin) of
        {ok, BranchSet} ->
            {ok, branch_set_dec(BranchSet)};
        {error, Reason} ->
            {error, Reason}
    end;
decode_branchset_json(Bin) ->
    error(badarg, [Bin]).


branch_set_enc([]) ->
    [];
branch_set_enc([{Slot, #branch{} = Branch} | R]) ->
    Seq = #'BranchSet_SEQOF'{slot = Slot, branch = wrap_branch(Branch)},
    [Seq | branch_set_enc(R)].

branch_set_dec(L) ->
    [ {Slot, unwrap_branch(Branch)} || #'BranchSet_SEQOF'{slot = Slot, branch = Branch} <- L].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Block
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% encode_block/1 -------------------------------------------------------------

-spec encode_block(Block :: #block{}) ->
    {ok, binary()} | {error, term()}.

encode_block(#block{} = B) ->
    'CoinProto':encode('Block', wrap_block(B));
encode_block(B) ->
    error(badarg, [B]).

%% decode_block/1 -------------------------------------------------------------

-spec decode_block(Bin :: binary()) ->
    {ok, #block{}} | {error, term()}.

decode_block(Bin) when is_binary(Bin) ->
    case 'CoinProto':decode('Block', Bin) of
        {ok, Block} ->
            {ok, unwrap_block(Block)};
        {error, Reason} ->
            {error, Reason}
    end;
decode_block(Bin) ->
    error(badarg, [Bin]).

%% encode_block_json/1 --------------------------------------------------------

-spec encode_block_json(Block :: #block{}) ->
    {ok, binary()} | {error, term()}.

encode_block_json(#block{} = B) ->
    'CoinProto':jer_encode('Block', wrap_block(B));
encode_block_json(B) ->
    error(badarg, [B]).

%% decode_block_json/1 --------------------------------------------------------

-spec decode_block_json(Bin :: binary()) ->
    {ok, #block{}} | {error, term()}.

decode_block_json(Bin) when is_binary(Bin) ->
    case 'CoinProto':jer_decode('Block', Bin) of
        {ok, Block} ->
            {ok, unwrap_block(Block)};
        {error, Reason} ->
            {error, Reason}
    end;
decode_block_json(Bin) ->
    error(badarg, [Bin]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WRAPPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Block ----------------------------------------------------------------------

wrap_block(#block{
    version = Vsn,
    index = Index,
    hash = Hash,
    prev_hash = PrevHash,
    timestamp = Timestamp,
    txs = OptionalTxs,
    nonce = Nonce,
    difficulty = Diff
}) ->
    #'Block'{
        version = Vsn,
        index = Index,
        hash = encode_optional(Hash),
        'prev-hash' = PrevHash,
        timestamp = Timestamp,
        transactions = optional_then(
            OptionalTxs,
            fun(Txs) ->
                lists:map(fun wrap_transaction/1, Txs)
            end),
        nonce = Nonce,
        difficulty = Diff
    }.

unwrap_block(#'Block'{
    version = Vsn,
    index = Index,
    hash = Hash,
    'prev-hash' = PrevHash,
    timestamp = Timestamp,
    transactions = OptionalTxs,
    nonce = Nonce,
    difficulty = Diff
}) ->
    #block{
        version = Vsn,
        index = Index,
        hash = decode_optional(Hash),
        prev_hash = PrevHash,
        timestamp = Timestamp,
        txs = optional_then(
            OptionalTxs,
            fun (Txs) ->
                lists:map(fun unwrap_transaction/1, Txs)
            end),
        nonce = Nonce,
        difficulty = Diff
    }.

%% Transaction ----------------------------------------------------------------

wrap_transaction(#tx{
    hash = Hash,
    in = TransactionIn,
    out = TransactionOut
}) ->
    #'Transaction'{
        hash = Hash,
        in = lists:map(fun wrap_transaction_in/1, TransactionIn),
        out = lists:map(fun wrap_transaction_out/1, TransactionOut)
    }.

unwrap_transaction(#'Transaction'{
    hash = Hash,
    in = TransactionIn,
    out = TransactionOut
}) ->
    #tx{
        hash = Hash,
        in = lists:map(fun unwrap_transaction_in/1, TransactionIn),
        out = lists:map(fun unwrap_transaction_out/1, TransactionOut)
    }.

%% TransactionIn --------------------------------------------------------------

wrap_transaction_in(#tx_in{
    out_hash = OutHash,
    out_index = OutIndex,
    signature = Signature
}) ->
    #'TransactionIn'{
        'out-hash' = encode_optional(OutHash),
        'out-index' = OutIndex,
        signature = encode_optional(Signature)
    }.

unwrap_transaction_in(#'TransactionIn'{
    'out-hash' = OutHash,
    'out-index' = OutIndex,
    signature = Signature
}) ->
    #tx_in{
        out_hash = decode_optional(OutHash),
        out_index = OutIndex,
        signature = decode_optional(Signature)
    }.

%% TransactionOut -------------------------------------------------------------

wrap_transaction_out(#tx_out{
    address = Address,
    amount = Amount
}) ->
    #'TransactionOut'{
        address = Address,
        amount = Amount
    }.

unwrap_transaction_out(#'TransactionOut'{
    address = Address,
    amount = Amount
}) ->
    #tx_out{
        address = Address,
        amount = Amount
    }.

%% Branch ---------------------------------------------------------------------

wrap_branch(#branch{
    latest_hash = LatestHash,
    cumulative_difficulty = CumulativeDiff,
    current_difficulty = CurrentDiff,
    length = Len,
    coinbase = Coinbase,
    supply = Supply
}) ->
    #'Branch'{
        'latest-hash' = LatestHash,
        'cumulative-difficulty' = CumulativeDiff,
        'current-difficulty' = CurrentDiff,
        length = Len,
        coinbase = Coinbase,
        supply = Supply
    }.
unwrap_branch(#'Branch'{
    'latest-hash' = LatestHash,
    'cumulative-difficulty' = CumulativeDiff,
    'current-difficulty' = CurrentDiff,
    length = Len,
    coinbase = Coinbase,
    supply = Supply
}) ->
    #branch{
        latest_hash = LatestHash,
        cumulative_difficulty = CumulativeDiff,
        current_difficulty = CurrentDiff,
        length = Len,
        coinbase = Coinbase,
        supply = Supply
    }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_optional(undefined) ->
    asn1_NOVALUE;
encode_optional(V) ->
    V.

decode_optional(asn1_NOVALUE) ->
    undefined;
decode_optional(V) ->
    V.

optional_then(asn1_NOVALUE, _Fn) ->
    undefined;
optional_then(undefined, _) ->
    asn1_NOVALUE;
optional_then(V, Fn) ->
    Fn(V).


-ifdef(TEST).

% ber: 393ms 24MB
% per: 341ms, 23MB
% uper: 235ms, 22MB

block_encode_decode_test() ->
    Block = fake_block(),
    % binary
    {ok, Bin} = encode_block(Block),
    {ok, Block} = decode_block(Bin),
    % json
    {ok, Json} = encode_block_json(Block),
    {ok, Block} = decode_block_json(Json),
    ok.

%% fakers

fake_block() ->
    #block{
        version = 1,
        index = 1,
        hash = fake_opt_hash(),
        prev_hash = fake_hash(),
        timestamp = erlang:system_time(millisecond),
        txs = fake_opt_transactions(),
        nonce = 0,
        difficulty = 0
    }.

fake_opt_transactions() ->
    case coin_flip() of
        true ->
            fake_transactions();
        false ->
            undefined
    end.

fake_transactions() ->
    [fake_transaction() || _ <- rand_range(128)].

fake_transaction() ->
    #tx{
        hash = fake_hash(),
        in = fake_transaction_in(rand:uniform(128)),
        out = fake_transaction_out(rand:uniform(128))
    }.

fake_transaction_in(0) ->
    [];
fake_transaction_in(N) when N rem 8 == 0 ->
    [#tx_in{out_index = 1} | fake_transaction_in(N-1)];
fake_transaction_in(N) ->
    [#tx_in{
        out_hash = fake_hash(),
        out_index = 1,
        signature = fake_signature()
     } | fake_transaction_in(N-1) ].

fake_transaction_out(0) ->
    [];
fake_transaction_out(N) ->
    [ #tx_out{
        address = fake_public_key(),
        amount = fake_amount()
    } | fake_transaction_out(N-1)].


fake_amount() ->
    rand:uniform(?COINBASE_AMOUNT)-1.
fake_hash() ->
    crypto:strong_rand_bytes(64).
fake_opt_hash() ->
    case coin_flip() of
        true ->
            undefined;
        false ->
            crypto:strong_rand_bytes(64)
    end.
fake_public_key() ->
    crypto:strong_rand_bytes(32).
fake_signature() ->
    crypto:strong_rand_bytes(64).

coin_flip() ->
    Flip = rand:uniform(2)-1,
    Flip == 0.

rand_range(E) ->
    lists:seq(1, rand:uniform(E)).

-endif.
