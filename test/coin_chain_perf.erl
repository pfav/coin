-module(coin_chain_perf).
-author("pfav").

-compile([export_all, nowarn_export_all]).

-include_lib("coin/include/coin.hrl").

-define(ACCOUNTS, (1000)).
-define(MAX_TX, (64)).

add(N) ->
    have_tab() orelse init(),
    add_blocks(N).

%% INIT

init() ->
    shell:catch_exception(true),
    io:format("initializing~n"),
    init_tab(),
    case csv_exists() of
        true ->
            io:format("loading keys~n"),
            csv_load();
        false ->
            io:format("initializing chain~n"),
            Keypair = coin_key:get_key(),
            ets:insert(?MODULE, Keypair),
            [true = ets:insert(?MODULE, coin_key:new_keypair())
                || _ <- lists:seq(1, ?ACCOUNTS-1)],
            csv_dump()
    end.

%% KEY.DB

have_tab() ->
    is_reference(ets:whereis(?MODULE)).

set_key() ->
    [{_, #keypair{priv = Priv}}] = ets:lookup(?MODULE, 0),
    coin_key:set_key(Priv).

init_tab() ->
    case have_tab() of
        false ->
            ets:new(?MODULE, [ordered_set, public, named_table, {keypos, #keypair.priv}]);
        true ->
            ets:delete_all_objects(?MODULE)
    end.

dump_tab() ->
    ets:tab2list(?MODULE).

% BLOCK

add_blocks(N) ->
    Fn = fun (I) ->
        {TxNum, MinedBlock} = mine_block(),
        T_insert_0 = erlang:monotonic_time(millisecond),
        ok = coin_chain:append(MinedBlock),
        T_insert_1 = erlang:monotonic_time(millisecond),
        io:format(standard_error, "verify: [~.6w][~.4w] ~wms~n", [I, TxNum, T_insert_1 - T_insert_0]),
        timer:sleep(1000)
        end,
    lists:foreach(Fn, lists:seq(1, N)).

mine_block() ->
    Miner = get_receiver(),
    Block0 = latest_block(),
    Block1 = coin_block:next(Block0),

    Coinbase = coin_tx:coinbase(Miner#keypair.pub, Block1#block.index),
    {N, Txs} = transactions(),
    MinedBlock = coin_block:mine(Block1#block{txs = [Coinbase|Txs]}),
    {N+1,  MinedBlock}.

%% TX

transactions() ->
    ets:foldl(fun transaction/2, {0, []}, ?MODULE).

transaction(Sk, {N, Acc}) when N < ?MAX_TX ->
    U0 = coin_tx:utxo(latest_tip(), Sk#keypair.pub),
    U1 = maps:filter(fun (_, A) -> A > 0 end, U0),
    case rand_elem(maps:to_list(U1)) of
        undefined ->
            {N, Acc};
        {{TxHash, Idx}, UAmount} ->
            TxAmount = UAmount div 2,
            Rk = get_receiver(),

            Tx0 = coin_tx:new(),
            Tx1 = coin_tx:add_input(Sk#keypair.priv, TxHash, Idx, Tx0),
            Tx2 = coin_tx:add_output(Sk#keypair.pub, TxAmount, Tx1),
            Tx3 = coin_tx:add_output(Rk#keypair.pub, TxAmount, Tx2),
            Tx4 = coin_tx:hash_it(Tx3),
            {N+1, [Tx4 | Acc]}
    end;
transaction(_, Acc) ->
    Acc.

get_receiver() ->
    rand_elem(dump_tab()).

rand_elem([]) ->
    undefined;
rand_elem(L) ->
    lists:nth(rand:uniform(length(L)), L).


%% CSV

csv_exists() ->
    filelib:is_file("keys.csv").

csv_load() ->
    {ok, F} = file:open("keys.csv", [read, binary]),
    _ = io:get_line(F, ""), % csv header
    csv_load(io:get_line(F, ""), F).

csv_load(eof, F) ->
    file:close(F);
csv_load(Line, F) ->
    [Priv, Pub] = binary:split(string:trim(Line), <<"|">>, [global]),
    {ok, PrivBin} = coin_address:decode_private(Priv),
    true = ets:insert_new(?MODULE,
        #keypair{
            priv = PrivBin,
            pub = binary:decode_hex(Pub)
        }
    ),
    csv_load(io:get_line(F, ""), F).

csv_dump() ->
    file:write_file("keys.csv", [
        io_lib:format("private_key|public_key~n", [])
        | [ io_lib:format("~s|~s~n", [
            coin_address:encode_private(Priv), binary:encode_hex(Pub)
        ]) || #keypair{priv = Priv, pub = Pub} <- dump_tab()
        ]
    ]).


%% CHAIN

%init_with_genesis() ->
    %Keypair = coin_key:new_keypair(),
    %Genesis = coin_block:genesis(Keypair#keypair.pub),
    %ok = coin_chain:append(Genesis).

latest_tip() ->
    {ok, ChainTip} = coin_chain:latest_hash(),
    ChainTip.

latest_block() ->
    {ok, LatestBlock} = coin_chain:latest(),
    LatestBlock.

latest_coinbase() ->
    LatestBlock = latest_block(),
    lists:nth(1, LatestBlock#block.txs).

