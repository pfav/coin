-module(chain_SUITE).
-author("pfav").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("coin/include/coin.hrl").

-compile([export_all, nowarn_export_all]).

suite() ->
    [{timetrap, {seconds, 600}}].

all() -> [
    {group, coin_db},
    {group, coin_branch},
    {group, coin_tx}
].

groups() ->
    [
        {coin_db, [sequence], [
            ct_db_each,
            ct_db_each_stop,
            ct_db_each_search,
            ct_db_fold
        ]},

        {coin_branch, [sequence], [
            ct_branch_get_set_remove,
            ct_branch_insert,
            ct_branch_find,
            ct_branch_all
        ]},

        {coin_tx, [sequence], [
            ct_tx_creation,
            %% coinbase
            ct_tx_invalid_only_coinbase,
            ct_tx_invalid_coinbase,
            ct_tx_invalid_coinbase_not_unique,
            ct_tx_invalid_multiple_coinbase,
            % transaction
            ct_tx_invalid_tx,
            ct_tx_invalid_not_unique,
            ct_tx_invalid_double_spend,
            ct_tx_invalid_double_spend_in_pending_block,
            % fork
            ct_fork,
            ct_fork_invalid_output
        ]}
    ].

init_per_suite(Config) ->
    ok = coin_db:open(<<"coin.db">>),
    setup_fake_chain(),
    Config.
end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.
end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    % clear all branches
    coin_db:clear(),
    Config.
end_per_testcase(_TestCase, Config) ->
    Config.


%% COIN_DB

ct_db_each(_) ->
    % prefix 0
    T0 = db_set_range(100, <<0>>),

    % prefix 1
    T1 = db_set_range(100, <<1>>),

    % iterate all with prefix 0
    C0 = counters:new(1, []),
    Res0 = coin_db:each(
        fun ({<<I>>, Val}) ->
            counters:add(C0, 1, 1),
            ?assertEqual(Val, ets:lookup_element(T0, I, 2))
        end, <<0>>),
    ?assertEqual(ok, Res0),
    ?assertEqual(100, counters:get(C0, 1)),

    % iterate all with prefix 0
    C1 = counters:new(1, []),
    Res1 = coin_db:each(
        fun ({<<I>>, Val}) ->
            counters:add(C1, 1, 1),
            ?assertEqual(Val, ets:lookup_element(T1, I, 2))
        end, <<1>>),
    ?assertEqual(ok, Res1),
    ?assertEqual(100, counters:get(C1, 1)),

    ok.

ct_db_each_stop(_) ->
    db_set_range(100, <<0>>),
    db_set_range(100, <<1>>),

    % iterate and stop early
    C = counters:new(1, []),
    Res = coin_db:each(fun ({<<I>>, _}) ->
        if
            I < 10 ->
                counters:add(C, 1, 1);
            true -> stop
        end
                        end, <<0>>),
    ?assertEqual(ok, Res),
    ?assertEqual(10, counters:get(C, 1)),

    ok.

ct_db_each_search(_) ->
    db_set_range(100, <<0>>),
    db_set_range(100, <<1>>),

    % iterate, stop early and return value
    C1 = counters:new(1, []),
    Res1 = coin_db:each(fun ({<<I>>, _}) ->
        if
            I < 10 ->
                counters:add(C1, 1, 1);
            true ->
                {stop, <<"yes">>}
        end
                        end, <<0>>),
    ?assertEqual({ok, <<"yes">>}, Res1),
    ?assertEqual(10, counters:get(C1, 1)),

    ok.

ct_db_fold(_) ->
    ok.

%% COIN_BRANCH

ct_branch_get_set_remove(_) ->
    % bad_input/get
    ?assertError(badarg, coin_branch:lookup(-1)),
    ?assertError(badarg, coin_branch:lookup(256)),
    ?assertError(badarg, coin_branch:lookup(256)),
    ?assertError(badarg, coin_branch:lookup("0")),
    ?assertError(badarg, coin_branch:lookup(<<>>)),

    % bad_input/set

    ?assertError(badarg, coin_branch:insert(-1, rand_branch())),
    ?assertError(badarg, coin_branch:insert(256, rand_branch())),
    ?assertError(badarg, coin_branch:insert(256, rand_branch())),
    ?assertError(badarg, coin_branch:insert("0", rand_branch())),
    ?assertError(badarg, coin_branch:insert(<<>>, rand_branch())),
    ?assertError(badarg, coin_branch:insert(0, <<>>)),

    % bad_input/remove
    ?assertError(badarg, coin_branch:remove(-1)),
    ?assertError(badarg, coin_branch:remove(256)),
    ?assertError(badarg, coin_branch:remove(256)),
    ?assertError(badarg, coin_branch:remove("0")),
    ?assertError(badarg, coin_branch:remove(<<>>)),

    % get nothing
    lists:foreach(fun (I) ->
        ?assertEqual({error, not_found}, coin_branch:lookup(I))
        end, lists:seq(0, 255)),

    % set/get
    B0 = rand_branch(),
    ?assertEqual(ok, coin_branch:insert(0, B0)),
    ?assertEqual({ok, B0}, coin_branch:lookup(0)),

    ?assertEqual(ok, coin_branch:insert(0, B0)),
    ?assertEqual({ok, B0}, coin_branch:lookup(0)),

    ?assertEqual({ok, [{0, B0}]}, coin_branch:all()),

    % set-all
    Tab = ets:new(tab, []),
    lists:foreach(fun (I) ->
        Branch = rand_branch(),
        ets:insert(Tab, {I, Branch}),
        ?assertEqual(ok, coin_branch:insert(I, Branch))
                  end, lists:seq(0, 255)),

    % get-all
    lists:foreach(fun (I) ->
        [{I, TabBranch}] = ets:lookup(Tab, I),
        {ok, Branch} = coin_branch:lookup(I),
        ?assertEqual(TabBranch, Branch)
                  end, lists:seq(0, 255)),

    ets:delete_all_objects(Tab),

    % remove-all
    lists:foreach(fun (I) ->
        ?assertEqual(ok, coin_branch:remove(I))
                  end, lists:seq(0, 255)),

    % get nothing
    lists:foreach(fun (I) ->
        ?assertEqual({error, not_found}, coin_branch:lookup(I))
                  end, lists:seq(0, 255)),
    ok.

ct_branch_insert(_) ->
    % bad_input/set
    ?assertError(badarg, coin_branch:append(0)),
    ?assertError(badarg, coin_branch:append("")),

    % append0
    B0 = rand_branch(),
    ?assertEqual(ok, coin_branch:append(B0)),
    ?assertEqual({ok, B0}, coin_branch:lookup(0)),
    % append1
    B1 = rand_branch(),
    ?assertEqual(ok, coin_branch:append(B1)),
    ?assertEqual({ok, B1}, coin_branch:lookup(1)),

    ok = coin_branch:remove(0),
    ok = coin_branch:remove(1),

    % don't insert the same value in new slot
    ?assertEqual(ok, coin_branch:append(B0)),
    ?assertEqual({error, already_present}, coin_branch:append(B0)),
    ok = coin_branch:remove(0),

    % append-all
    lists:foreach(fun (I) ->
        Branch = rand_branch(),
        ?assertEqual(ok, coin_branch:append(Branch)),
        ?assertEqual({ok, Branch}, coin_branch:lookup(I))
                  end, lists:seq(0, 255)),

    ok.

ct_branch_find(_) ->
    % bad_input/set
    ?assertError(badarg, coin_branch:find(0)),
    ?assertError(badarg, coin_branch:find("")),

    % append/find
    Branch = rand_branch(),
    ?assertEqual({error, not_found}, coin_branch:find(Branch#branch.latest_hash)),
    ?assertEqual(ok, coin_branch:append(Branch)),
    ?assertEqual({ok, 0}, coin_branch:find(Branch#branch.latest_hash)),

    % append/find same key
    ?assertEqual({error, already_present}, coin_branch:append(Branch)),
    ?assertEqual({ok, 0}, coin_branch:find(Branch#branch.latest_hash)),
    ok.

ct_branch_all(_) ->
    ?assertEqual({ok, []}, coin_branch:all()),

    Tab = ets:new(tab, []),
    lists:foreach(fun (I) ->
        Branch = rand_branch(),
        ets:insert(Tab, {I, Branch}),
        ?assertEqual(ok, coin_branch:append(Branch))
                  end, lists:seq(0, 255)),

    {ok, L} = coin_branch:all(),
    lists:foreach(fun ({I, B}) ->
        [{I, TabBranch}] = ets:lookup(Tab, I),
        ?assertEqual(TabBranch, B)
                  end, L),
    ok.

%% PRIVATE

rand_branch() ->
    #branch{
        latest_hash = rand:bytes(64),
        current_difficulty = 1,
        cumulative_difficulty = 1,
        length = 1,
        coinbase = 10,
        supply = 1
    }.

db_set_range(N, Prefix) ->
    Tab = ets:new(tab, []),
    Fn =
        fun(I) ->
            Val = crypto:strong_rand_bytes(12),
            ok = coin_db:insert(<<Prefix/binary, I>>, Val),
            true = ets:insert(Tab, {I, Val})
         end,
    lists:foreach(Fn, lists:seq(0, N-1)),
    Tab.

%% COIN_TX %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_tx_creation(_) ->
    Ak = init_with_genesis(),
    Bk = coin_key:new_keypair(),

    ChainTip0 = latest_tip(0),
    Block0 = latest_block(0),
    Coinbase0 = latest_coinbase(0),

    ?assertEqual(50 * ?CID, coin_tx:balance(ChainTip0, Ak#keypair.pub)), % A = 50 - 10 + 50(coinbase)
    ?assertEqual( 0 * ?CID, coin_tx:balance(ChainTip0, Bk#keypair.pub)), % B = 10

    % Block#0001
    Block1 = coin_block:next(Block0),
    % A +50 Coin
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, Block1#block.index),

    % A send 10 Coin to B
    A0 = coin_tx:new(),
    A1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, A0),
    A2 = coin_tx:add_output(Bk#keypair.pub, 10 * ?CID, A1),
    A3 = coin_tx:add_output(Ak#keypair.pub, 40 * ?CID, A2),
    A4 = coin_tx:hash_it(A3),

    MinedBlock1 = coin_block:mine(Block1#block{txs = [Coinbase1, A4]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock1)),

    % Balance
    ?assertEqual(MinedBlock1, latest_block(0)),
    ?assertEqual(MinedBlock1#block.hash, latest_tip(0)),
    ?assertEqual(Coinbase1, latest_coinbase(0)),

    ?assertEqual(90 * ?CID, coin_tx:balance(latest_tip(0), Ak#keypair.pub)), % A = 50 - 10 + 50(coinbase)
    ?assertEqual(10 * ?CID, coin_tx:balance(latest_tip(0), Bk#keypair.pub)), % B = 10

    % Block#0002
    Block2 = coin_block:next(MinedBlock1),
    % B +50 Coin
    Coinbase2 = coin_tx:coinbase(Bk#keypair.pub, Block2#block.index),

    % B send 10 Coin to B
    B0 = coin_tx:new(),
    B1 = coin_tx:add_input(Bk#keypair.priv, A4#tx.hash, 0, B0),
    B2 = coin_tx:add_output(Bk#keypair.pub, 10 * ?CID, B1),
    B3 = coin_tx:hash_it(B2),

    MinedBlock2 = coin_block:mine(Block2#block{txs = [Coinbase2, B3]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock2)),

    ?assertEqual(90 * ?CID, coin_tx:balance(latest_tip(0), Ak#keypair.pub)), % A = 90
    ?assertEqual(60 * ?CID, coin_tx:balance(latest_tip(0), Bk#keypair.pub)), % B = 10 + 50(coinbase)

    ok.

%% coinbase %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_tx_invalid_only_coinbase(_) ->
    Ak = init_with_genesis(),
    Block0 = latest_block(0),

    Block1 = coin_block:next(Block0),
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, Block1#block.index),

    MinedBlock = coin_block:mine(Block1#block{txs = [Coinbase1]}),
    ?assertEqual({error,[is_valid_transaction,is_txs_valid_size]}, coin_chain:append(MinedBlock)),
    ok.

ct_tx_invalid_coinbase(_) ->
    Ak = init_with_genesis(),
    ChainTip0 = latest_tip(0),
    LatestBlock = latest_block(0),

    CurIndex = LatestBlock#block.index,
    NextIndex = LatestBlock#block.index + 1,
    Coinbase = coin_tx:coinbase(Ak#keypair.pub, CurIndex),

    % A send 10 Coin to B
    T0 = coin_tx:new(),
    T1 = coin_tx:add_input(Ak#keypair.priv, Coinbase#tx.hash, 0, T0),
    T2 = coin_tx:add_output(Ak#keypair.pub, 50 * ?CID, T1),
    T3 = coin_tx:hash_it(T2),

    ?assertEqual({false, is_valid_coinbase_tx}, coin_tx:is_valid([Coinbase, T3], ChainTip0, NextIndex)),
    ok.

ct_tx_invalid_coinbase_not_unique(_) ->
    Ak = init_with_genesis(),
    ChainTip0 = latest_tip(0),
    LatestBlock = latest_block(0),

    NextBlockHeight = LatestBlock#block.index + 1,
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, NextBlockHeight),

    ?assertEqual({false,[is_valid_tx_each, is_valid_input, too_many_coinbase]}, coin_tx:is_valid([Coinbase1, Coinbase1], ChainTip0, NextBlockHeight)),
    ok.

ct_tx_invalid_multiple_coinbase(_) ->
    Ak = init_with_genesis(),
    Bk = coin_key:new_keypair(),
    ChainTip0 = latest_tip(0),
    LatestBlock = latest_block(0),

    NextBlockHeight = LatestBlock#block.index + 1,
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, NextBlockHeight),
    Coinbase2 = coin_tx:coinbase(Bk#keypair.pub, NextBlockHeight),

    ?assertEqual({false,[is_valid_tx_each, is_valid_input, too_many_coinbase]}, coin_tx:is_valid([Coinbase1, Coinbase2], ChainTip0, NextBlockHeight)),
    ok.

%% transaction %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_tx_invalid_tx(_) ->
    Ak = init_with_genesis(),
    ChainTip0 = latest_tip(0),
    LatestBlock = latest_block(0),

    NextBlockHeight = LatestBlock#block.index + 1,
    Coinbase = coin_tx:coinbase(Ak#keypair.pub, NextBlockHeight),

    % A send 10 Coin to B
    T0 = coin_tx:new(),
    T1 = coin_tx:add_input(Ak#keypair.priv, Coinbase#tx.hash, 0, T0),
    T2 = coin_tx:add_output(Ak#keypair.pub, 50 * ?CID, T1),
    T3 = coin_tx:hash_it(T2),

    ?assertEqual({false, [is_valid_tx_each,is_valid_input,not_found]}, coin_tx:is_valid([T3], ChainTip0, NextBlockHeight)),
    ok.


ct_tx_invalid_not_unique(_) ->
    Ak = init_with_genesis(),
    ChainTip0 = latest_tip(0),
    LatestBlock = latest_block(0),
    Coinbase0 = latest_coinbase(0),

    NextBlockHeight = LatestBlock#block.index + 1,
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, NextBlockHeight),

    % A send 10 Coin to B
    T0 = coin_tx:new(),
    T1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, T0),
    T2 = coin_tx:add_output(Ak#keypair.pub, 50 * ?CID, T1),
    T3 = coin_tx:hash_it(T2),

    ?assertEqual(true, coin_tx:is_valid([Coinbase1, T3], ChainTip0, NextBlockHeight)),

    ?assertEqual({false,[is_txs_hash_unique]}, coin_tx:is_valid([Coinbase1, T3, T3], ChainTip0, NextBlockHeight)),
    ok.


ct_tx_invalid_double_spend(_) ->
    Ak = init_with_genesis(),
    Block0 = latest_block(0),
    Coinbase0 = latest_coinbase(0),

    Block1 = coin_block:next(Block0),
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, Block1#block.index),
    T0 = coin_tx:new(),
    T1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, T0),
    T2 = coin_tx:add_output(Ak#keypair.pub, 50 * ?CID, T1),
    T3 = coin_tx:hash_it(T2),

    MinedBlock1 = coin_block:mine(Block1#block{txs = [Coinbase1, T3]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock1)),

    Block2 = coin_block:next(MinedBlock1),
    Coinbase2 = coin_tx:coinbase(Ak#keypair.pub, Block2#block.index),

    % double spend (T3)
    MinedBlock2 = coin_block:mine(Block2#block{txs = [Coinbase2, T3]}),
    ?assertEqual({error,[
        is_valid_transaction,
        is_valid_tx_each,
        is_valid_not_double_spent]}, coin_chain:append(MinedBlock2)),
    ok.

ct_tx_invalid_double_spend_in_pending_block(_) ->
    Ak = init_with_genesis(),
    Block0 = latest_block(0),
    Coinbase0 = latest_coinbase(0),

    Block1 = coin_block:next(Block0),
    Coinbase1 = coin_tx:coinbase(Ak#keypair.pub, Block1#block.index),

    % first transaction
    Ax0 = coin_tx:new(),
    Ax1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, Ax0),
    Ax2 = coin_tx:add_output(Ak#keypair.pub, 50 * ?CID, Ax1),
    Ax3 = coin_tx:hash_it(Ax2),

    % second transaction
    Bx0 = coin_tx:new(),
    Bx1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, Bx0),
    Bx2 = coin_tx:add_output(Ak#keypair.pub, 10 * ?CID, Bx1),
    Bx3 = coin_tx:hash_it(Bx2),

    MinedBlock = coin_block:mine(Block1#block{txs = [Coinbase1, Ax3, Bx3]}),
    ?assertEqual({error,[is_valid_transaction,is_txs_input_unique]}, coin_chain:append(MinedBlock)),

    ok.

%% fork %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_fork(_) ->
    Ak = init_with_genesis(),
    Bk = coin_key:new_keypair(),

    ChainTip0 = latest_tip(0),
    Block0 = latest_block(0),
    Coinbase0 = latest_coinbase(0),

    ?assertEqual(50 * ?CID, coin_tx:balance(ChainTip0, Ak#keypair.pub)),
    ?assertEqual( 0 * ?CID, coin_tx:balance(ChainTip0, Bk#keypair.pub)),

    %% BRANCH_0
    Block_0_1 = coin_block:next(Block0),
    Coinbase_0_1 = coin_tx:coinbase(Ak#keypair.pub, Block_0_1#block.index),
    A0 = coin_tx:new(),
    A1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, A0),
    A2 = coin_tx:add_output(Bk#keypair.pub, 10 * ?CID, A1),
    A3 = coin_tx:add_output(Ak#keypair.pub, 40 * ?CID, A2),
    A4 = coin_tx:hash_it(A3),

    MinedBlock_0_1 = coin_block:mine(Block_0_1#block{txs = [Coinbase_0_1, A4]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock_0_1)),

    ?assertEqual(90 * ?CID, coin_tx:balance(latest_tip(0), Ak#keypair.pub)),
    ?assertEqual(10 * ?CID, coin_tx:balance(latest_tip(0), Bk#keypair.pub)),

    %% BRANCH_1
    Block_1_1 = coin_block:next(Block0),
    Coinbase_1_1 = coin_tx:coinbase(Bk#keypair.pub, Block_1_1#block.index),
    B0 = coin_tx:new(),
    B1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, B0),
    B2 = coin_tx:add_output(Bk#keypair.pub, 50 * ?CID, B1),
    B3 = coin_tx:hash_it(B2),

    MinedBlock_1_1 = coin_block:mine(Block_1_1#block{txs = [Coinbase_1_1, B3]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock_1_1)),

    ?assertEqual(0 * ?CID, coin_tx:balance(latest_tip(1), Ak#keypair.pub)),
    ?assertEqual(100 * ?CID, coin_tx:balance(latest_tip(1), Bk#keypair.pub)),

    Block_1_2 = coin_block:next(latest_block(1)),
    Coinbase_1_2 = coin_tx:coinbase(Ak#keypair.pub, Block_1_2#block.index),
    C0 = coin_tx:new(),
    C1 = coin_tx:add_input(Bk#keypair.priv, Coinbase_1_1#tx.hash, 0, C0),
    C2 = coin_tx:add_input(Bk#keypair.priv, B3#tx.hash, 0, C1),
    C3 = coin_tx:add_output(Bk#keypair.pub, 60 * ?CID, C2),
    C4 = coin_tx:add_output(Ak#keypair.pub, 40 * ?CID, C3),
    C5 = coin_tx:hash_it(C4),

    MinedBlock_1_2 = coin_block:mine(Block_1_2#block{txs = [Coinbase_1_2, C5]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock_1_2)),

    ?assertEqual(90 * ?CID, coin_tx:balance(latest_tip(1), Ak#keypair.pub)),
    ?assertEqual(60 * ?CID, coin_tx:balance(latest_tip(1), Bk#keypair.pub)),
    ok.


ct_fork_invalid_output(_) ->
    % invalid to spend output from another branch
    Ak = init_with_genesis(),
    Bk = coin_key:new_keypair(),

    ChainTip0 = latest_tip(0),
    Block0 = latest_block(0),
    Coinbase0 = latest_coinbase(0),

    ?assertEqual(50 * ?CID, coin_tx:balance(ChainTip0, Ak#keypair.pub)),
    ?assertEqual( 0 * ?CID, coin_tx:balance(ChainTip0, Bk#keypair.pub)),

    %% BRANCH_0
    Block_0_1 = coin_block:next(Block0),
    Coinbase_0_1 = coin_tx:coinbase(Ak#keypair.pub, Block_0_1#block.index),
    A0 = coin_tx:new(),
    A1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, A0),
    A2 = coin_tx:add_output(Bk#keypair.pub, 10 * ?CID, A1),
    A3 = coin_tx:add_output(Ak#keypair.pub, 40 * ?CID, A2),
    A4 = coin_tx:hash_it(A3),

    MinedBlock_0_1 = coin_block:mine(Block_0_1#block{txs = [Coinbase_0_1, A4]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock_0_1)),

    ?assertEqual(90 * ?CID, coin_tx:balance(latest_tip(0), Ak#keypair.pub)),
    ?assertEqual(10 * ?CID, coin_tx:balance(latest_tip(0), Bk#keypair.pub)),

    %% BRANCH_1
    Block_1_1 = coin_block:next(Block0),
    Coinbase_1_1 = coin_tx:coinbase(Bk#keypair.pub, Block_1_1#block.index),
    B0 = coin_tx:new(),
    B1 = coin_tx:add_input(Ak#keypair.priv, Coinbase0#tx.hash, 0, B0),
    B2 = coin_tx:add_output(Bk#keypair.pub, 50 * ?CID, B1),
    B3 = coin_tx:hash_it(B2),

    MinedBlock_1_1 = coin_block:mine(Block_1_1#block{txs = [Coinbase_1_1, B3]}),
    ?assertEqual(ok, coin_chain:append(MinedBlock_1_1)),

    ?assertEqual(0 * ?CID, coin_tx:balance(latest_tip(1), Ak#keypair.pub)),
    ?assertEqual(100 * ?CID, coin_tx:balance(latest_tip(1), Bk#keypair.pub)),

    Block_1_2 = coin_block:next(latest_block(1)),
    Coinbase_1_2 = coin_tx:coinbase(Ak#keypair.pub, Block_1_2#block.index),

    % invalid input: coinbase from other branch
    C0 = coin_tx:new(),
    C1 = coin_tx:add_input(Ak#keypair.priv, Coinbase_0_1#tx.hash, 0, C0),
    C2 = coin_tx:add_output(Bk#keypair.pub, 40 * ?CID, C1),
    C3 = coin_tx:hash_it(C2),

    MinedBlock_1_2_0 = coin_block:mine(Block_1_2#block{txs = [Coinbase_1_2, C3]}),
    ?assertEqual({error,[
        is_valid_transaction,
        is_valid_tx_each,
        is_valid_input,
        not_found]}, coin_chain:append(MinedBlock_1_2_0)),

    % invalid input: tx from other branch
    D0 = coin_tx:new(),
    D1 = coin_tx:add_input(Bk#keypair.priv, A4#tx.hash, 0, D0),
    D2 = coin_tx:add_output(Ak#keypair.pub, 10 * ?CID, D1),
    D3 = coin_tx:hash_it(D2),

    MinedBlock_1_2_1 = coin_block:mine(Block_1_2#block{txs = [Coinbase_1_2, D3]}),
    ?assertEqual({error,[
        is_valid_transaction,
        is_valid_tx_each,
        is_valid_input,
        not_found]}, coin_chain:append(MinedBlock_1_2_1)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTIL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_fake_chain() ->
    % setup fake chain
    ok = meck:new(coin_chain, [non_strict, no_link, passthrough]),
    ok = meck:expect(coin_chain, append,
        fun(Block) ->
            case meck:passthrough([Block]) of
                ok ->
                    coin_chain:append_to_chain(Block);
                Result ->
                    Result
            end
        end),
    ?assert(meck:validate(coin_chain)),
    ok.

init_with_genesis() ->
    Keypair = coin_key:new_keypair(),
    Genesis = coin_block:genesis(Keypair#keypair.pub),
    ?assertEqual(ok, coin_chain:append(Genesis)),
    Keypair.

latest_tip(Slot) ->
    {ok, #branch{latest_hash = ChainTip}} = coin_branch:lookup(Slot),
    ChainTip.

latest_block(Slot) ->
    {ok, LatestBlock} = coin_chain:latest(Slot),
    LatestBlock.

latest_coinbase(Slot) ->
    LatestBlock = latest_block(Slot),
    lists:nth(1, LatestBlock#block.txs).


%% chain fake process




