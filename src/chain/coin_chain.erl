-module(coin_chain).
-author("pfav").

-behaviour(gen_statem).

%% API
-export([
    start_link/0,

    append/1,
    info/1,
    latest/1,
    latest/0,
    latest_hash/0,
    size/1,
    size/0,

    current_difficulty/1,
    cumulative_difficulty/1,
    coinbase_amount/1,
    is_valid/1,
    walk/3,

    handle_http/4
]).


%% gen_statem callbacks
-export([
    init/1,
    active/3,
    terminate/3,
    code_change/4,
    callback_mode/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("coin/include/coin.hrl").

-record(data, {}).

%%%============================================================================
%%% API
%%%============================================================================

%% start_link/0 ---------------------------------------------------------------

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% append/1 -------------------------------------------------------------------

-spec append(Block :: coin_block:block())
            -> ok | {error, term()}.

append(Block) ->
    case coin_block:member(Block#block.hash) of
        true ->
            {error, already_present};
        false ->
            append_block(Block)
    end.

append_block(#block{index = 0} = Block) ->
    validate_and_append(undefined, Block);
append_block(Block) ->
    case coin_block:prev(Block) of
        {ok, PrevBlock} ->
            validate_and_append(PrevBlock, Block);
        {error, not_found} ->
            {error, invalid}
    end.

validate_and_append(PrevBlock, Block) ->
    case coin_block:is_valid(PrevBlock, Block) of
        true ->
            gen_statem:cast(?MODULE, {append, Block});
        {false, Reason}->
            {error, Reason}
    end.



%% info/1 ---------------------------------------------------------------------

-spec info(Slot) -> {ok, Branch} | {error, term()}
    when
    Slot :: coin_branch:slot(),
    Branch :: coin_branch:branch().

info(Slot) ->
    coin_branch:lookup(Slot).

%% latest/1 -------------------------------------------------------------------

-spec latest(Slot) -> {ok, Block} | {error, term()}
    when
    Slot :: coin_branch:slot(),
    Block :: coin_block:block().

latest(Slot) ->
    case coin_branch:lookup(Slot) of
        {ok, #branch{latest_hash = ChainTip}} ->
            coin_block:lookup(ChainTip);
        {error, Reason} ->
            {error, Reason}
    end.

%% latest/0 -------------------------------------------------------------------

-spec latest() -> {ok, Block} | {error, term()}
    when
    Block :: coin_block:block().

latest() ->
    case coin_branch:best() of
        {ok, Slot} ->
            latest(Slot);
        {error, Reason} ->
            {error, Reason}
    end.

%% latest_hash/0 --------------------------------------------------------------

-spec latest_hash() -> {ok, Hash} | {error, term()}
    when
    Hash :: coin_block:hash().

latest_hash() ->
    case coin_branch:best() of
        {ok, Slot} ->
            case info(Slot) of
                {ok, #branch{latest_hash = Hash}} ->
                    {ok, Hash};
                _ ->
                    {error, not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% size/1 ---------------------------------------------------------------------

-spec size(Slot) -> {ok, Size} | {error, term()} when
    Slot :: coin_branch:slot(),
    Size :: non_neg_integer().

size(Slot) ->
    case latest(Slot) of
        {ok, #block{index = I}} ->
            {ok, I + 1};
        {error, Reason} ->
            {error, Reason}
    end.

%% size/0 ---------------------------------------------------------------------

-spec size() -> {ok, Size} | {error, term()} when
    Size :: non_neg_integer().

size() ->
    case latest() of
        {ok, #block{index = I}} ->
            {ok, I + 1};
        {error, Reason} ->
            {error, Reason}
    end.

%% current_difficulty/1 -------------------------------------------------------

-spec current_difficulty(Slot) -> {ok, Difficulty} | {error, term()} when
    Slot :: coin_branch:slot(),
    Difficulty :: non_neg_integer().

current_difficulty(Slot) ->
    case latest(Slot) of
        {ok, Block} ->
            if
                Block#block.index rem ?DIFFICULTY_ADJUSTMENT_INTERVAL == 0 andalso Block#block.index /= 0 ->
                    get_adjusted_difficulty(Block);
                true ->
                    Block#block.difficulty
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_adjusted_difficulty(LatestBlock) ->
    LatestIdx = LatestBlock#block.index,
    {ok, PrevAdjBlock} = coin_block:lookup_by_index(LatestIdx - ?DIFFICULTY_ADJUSTMENT_INTERVAL, LatestBlock#block.hash),

    TimeExpected = ?BLOCK_GENERATION_INTERVAL * ?DIFFICULTY_ADJUSTMENT_INTERVAL,
    TimeTaken = erlang:convert_time_unit(LatestBlock#block.timestamp - PrevAdjBlock#block.timestamp, millisecond, second),
    if
        TimeTaken < TimeExpected / 2 ->
            {ok, PrevAdjBlock#block.difficulty + 1};
        TimeTaken > TimeExpected * 2 ->
            {ok, min(PrevAdjBlock#block.difficulty - 1, 1)};
        true ->
            {ok, PrevAdjBlock#block.difficulty}
    end.


%% cumulative_difficulty/1 ----------------------------------------------------

% NAKAMOTO CONSENSUS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec cumulative_difficulty(SlotOrBlock) -> {ok, Consensus} | {error, term()} when
    SlotOrBlock :: coin_block:block() | coin_branch:slot(),
    Consensus :: non_neg_integer().

cumulative_difficulty(Slot) when is_integer(Slot) ->
    cumulative_difficulty(latest(Slot), 0);
cumulative_difficulty(Block = #block{}) ->
    cumulative_difficulty({ok, Block}, 0).

cumulative_difficulty({ok, #block{index = 0, difficulty = Diff}}, Sum) ->
    {ok, round(Sum + math:pow(2, Diff))};
cumulative_difficulty({ok, #block{difficulty = Diff} = B}, Sum) ->
    cumulative_difficulty(coin_block:prev(B), Sum + math:pow(2, Diff));
cumulative_difficulty({error, Reason}, _Sum) ->
    {error, Reason}.


%% coinbase_amount/1 ----------------------------------------------------------

-spec coinbase_amount(Index) -> Amount when
    Index :: non_neg_integer(),
    Amount :: non_neg_integer().

% coinbase amount based on block height
coinbase_amount(Index) ->
    ?COINBASE_AMOUNT div (1 + Index div ?COINBASE_GENERATION).

%% is_valid/1 -----------------------------------------------------------------

%% TODO: implement this to check

is_valid(Branch) when is_integer(Branch) ->
    ok;
is_valid(BlockHash) when is_binary(BlockHash) ->
    ok.

%% walk/3 ---------------------------------------------------------------------

-spec walk(Fn, Start, Count) -> Result | no_return() when
    Fn :: fun((#block{}) -> T),
    Result :: {ok, [T]} | {error, term()},
    Start :: coin_block:hash(),
    Count :: non_neg_integer().

%% walk chain
walk(Fn, Start, Count) when Count > 0 ->
    case coin_block:lookup(Start) of
        {ok, #block{} = Block} ->
            walk_limit({ok, Block}, Fn, Count, []);
        {error, Reason} ->
            {error, Reason}
    end;
walk(Fn, Start, Count) ->
    error(badarg, [Fn, Start, Count]).

walk_limit({ok, #block{prev_hash = <<0:512>>} = Block}, Fn, _Count, Acc) ->
    {ok, [Fn(Block) | Acc]};
walk_limit({ok, Block}, Fn, Count, Acc) when Count > 1 ->
    walk_limit(coin_block:prev(Block), Fn, Count - 1, [ Fn(Block) | Acc]);
walk_limit({ok, Block}, Fn, _Count, Acc)  ->
    {ok, [Fn(Block) | Acc]};
walk_limit({error, Reason}, _Fn, _Count, _Acc)->
    {error, Reason}.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
    ?LOG_NOTICE("[CHAIN] Initializing"),
    case coin_branch:all() of
        {ok, []} ->
            ?LOG_NOTICE("[CHAIN] initialize with genesis"),
            GenBlock = coin_block:genesis(),
            {ok, active, #data{}, [
                {next_event, internal, {append, GenBlock}}
            ]};
        {ok, _} ->
            ?LOG_INFO("[CHAIN] Started"),
            {ok, active, #data{}, []}
    end.


callback_mode() ->
    state_functions.

active(internal, {append, Block}, #data{}) ->
    T0 = erlang:monotonic_time(),
    case append_to_chain(Block) of
       ok ->
           T1 = erlang:monotonic_time(),
           ?LOG_INFO("[CHAIN] appended block ~w:~s in ~wms", [
               Block#block.index, coin_util:brief(Block#block.hash, 8),
               erlang:convert_time_unit(T1-T0, native, millisecond)
           ]),
           if
               Block#block.index =/= 0 ->
                   coin_peer:broadcast(Block);
               true -> skip
           end,
           {keep_state_and_data, []};
        {error, Reason} ->
            ?LOG_ERROR("[CHAIN] failed append block ~s reason ~p", [coin_util:brief(Block#block.hash, 8), Reason]),
            {keep_state_and_data, []}
    end;
active(cast, {append, #block{} = CandidateBlock}, #data{}) ->
    case coin_block:member(CandidateBlock#block.hash) of
        true ->
            ?LOG_NOTICE("[CHAIN] block ~s already_present", [coin_util:brief(CandidateBlock#block.hash, 8)]),
            {keep_state_and_data, []};
        false ->
            {keep_state_and_data, [
                {next_event, internal, {append, CandidateBlock}}
            ]}
    end;
active(Type, Content, D = #data{}) ->
    handle_common(Type, Content, ?FUNCTION_NAME, D).

handle_common(Type, Content, State, #data{}) ->
    ?LOG_WARNING("[CHAIN] unhandled event state ~p ~p ~p", [State, Type, Content]),
    {keep_state_and_data, []}.

terminate(_Reason, _StateName, _State = #data{}) ->
    ok.

code_change(_OldVsn, StateName, State = #data{}, _Extra) ->
    {ok, StateName, State}.


%%%===================================================================
%%% HTTP
%%%===================================================================

handle_http(<<"GET">>, <<"/api/blockchain/info">>, _Req, _Data) ->
    {ok, B0} = coin_branch:all(),
    {ok, Json} = coin_proto:encode_branchset_json(B0),
    {200, coin_http:headers(), Json};

handle_http(<<"GET">>, <<"/api/blockchain/:slot/info">>,  _Req, Data) ->
    Slot = maps:get(slot, Data),
    case coin_branch:lookup(Slot) of
        {ok, Branch} ->
            {ok, Json} = coin_proto:encode_branch_json(Branch),
            {200, coin_http:headers(), Json};
        {error, not_found} ->
            coin_http:error404()
    end;

handle_http(<<"GET">>, <<"/api/blockchain/:slot/latest">>, _Req, Data) ->
    Slot = maps:get(slot, Data),
    case latest(Slot) of
        {ok, Block} ->
            {ok, Bin} = coin_proto:encode_block_json(Block),
            {200, coin_http:headers(), Bin};
        {error, not_found} ->
            coin_http:error404()
    end;

handle_http(<<"GET">>, <<"/api/blockchain">>, Req, _Data) ->
    Qs = cowboy_req:match_qs([
        {start, [fun coin_http:constraint_valid_hash/2], undefined},
        {count, [int, fun coin_http:constraint_pos_integer/2], 10}
    ], Req),
    Start =
        case maps:get(start, Qs, undefined) of
            undefined ->
                {ok, Hash} = latest_hash(),
                Hash;
            Arg ->
                Arg
        end,
    Count = min(maps:get(count, Qs), 100),
    ?LOG_DEBUG("[CHAIHN] Chain walk start: ~p, count: ~w", [coin_util:brief(Start, 8), Count]),
    Fn = fun (#block{hash = Hash}) -> binary:encode_hex(Hash) end,
    case walk(Fn, Start, Count) of
        {ok, Blockchain} ->
            {200, coin_http:headers(), Blockchain};
        {error, not_found} ->
            coin_http:error404();
        {error, Reason} ->
            ?LOG_WARNING("[CHAIN] failed walk reason ~p", [Reason]),
            coin_http:error505()
    end;

handle_http(<<"POST">>, <<"/api/blockchain">>, _Req, Data) ->
    {ok, Block} = coin_proto:decode_block_json(jsx:encode(Data)),
    case append(Block) of
        ok ->
            {200, coin_http:headers(), #{result => ok}};
        {error, already_present} ->
            {200, coin_http:headers(), #{result => error, msg => already_present}}
    end;

handle_http(<<"GET">>, <<"/api/blockchain/:hash">>, _Req, Data) ->
    Hash = maps:get(hash, Data),
    case coin_block:lookup(Hash) of
        {ok, Block} ->
            {ok, Bin} = coin_proto:encode_block_json(Block),
            {200, coin_http:headers(), Bin};
        {error, not_found} ->
            coin_http:error404()
    end;

handle_http(<<"GET">>, <<"/api/blockchain/:index">>, _Req, Data) ->
    Index = maps:get(index, Data),
    {ok, ChainTip} = latest_hash(),
    case coin_block:lookup_by_index(Index, ChainTip) of
        {ok, Block} ->
            {ok, Bin} = coin_proto:encode_block_json(Block),
            {200, coin_http:headers(), Bin};
        {error, not_found} ->
            coin_http:error404()
    end;

handle_http(<<"GET">>, <<"/api/blockchain/latest">>, _Req, _Data) ->
    case latest() of
        {ok, Block} ->
            {ok, Bin} = coin_proto:encode_block_json(Block),
            {200, coin_http:headers(), Bin};
        {error, not_found} ->
            coin_http:error404()
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

append_to_chain(Block) ->
    % TODO: THIS SHOULD BE ATOMIC (cleanup if failure)
    case update_branch(Block) of
        ok ->
            coin_block:insert(Block);
        {error, Reason} ->
            {error, Reason}
    end.

update_branch(#block{index = 0} = Block) ->
    coin_branch:append(#branch{
        latest_hash = Block#block.hash,
        current_difficulty = Block#block.difficulty,
        cumulative_difficulty = round(math:pow(2, Block#block.difficulty)),
        length = Block#block.index + 1,
        coinbase = coinbase_amount(Block#block.index),
        supply = 0 % TODO
    });

update_branch(#block{prev_hash = PrevHash, hash = Hash} = B) ->
    case coin_branch:find(PrevHash) of
        {ok, Slot} ->
            {ok, #branch{
                cumulative_difficulty = CumDiff
            }} = coin_branch:lookup(Slot),
            coin_branch:insert(Slot, #branch{
                latest_hash = Hash,
                current_difficulty = B#block.difficulty,
                cumulative_difficulty = CumDiff + round(math:pow(2, B#block.difficulty)),
                length = B#block.index + 1,
                coinbase = coinbase_amount(B#block.index),
                supply = 0 % TODO
            });
        {error, not_found} ->
            {ok, D} = cumulative_difficulty(B),
            coin_branch:append(#branch{
                latest_hash = Hash,
                current_difficulty = B#block.difficulty,
                cumulative_difficulty = round(D),
                length = B#block.index + 1,
                coinbase = coinbase_amount(B#block.index),
                supply = 0 % TODO
            })
    end.

-ifdef(TEST).
-endif.