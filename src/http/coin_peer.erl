-module(coin_peer).
-author("pfav").

-author("pfav").

-behaviour(gen_statem).

%% API
-export([
    start_link/1,
    send_block/2,
    broadcast/1,

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

-include_lib("coin/include/coin.hrl").

-record(data, {
    name :: binary(),
    ip :: inet:ip_address(),
    port :: non_neg_integer()
}).

-define(POLL_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

% TODO: find a way not to send the block we just received here

start_link(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts, 9000),
    gen_statem:start_link(coin_reg:peer_key(Ip, Port), ?MODULE, Opts, []).

send_block(Pid, Block) ->
    gen_statem:cast(Pid, {send_block, Block}).

broadcast(Block) ->
    lists:foreach(
        fun (Pid) -> send_block(Pid, Block) end,
    coin_peer_sup:pids()).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts, 9000),
    Name = format_name(Ip, Port),
    check_not_self(Ip, Port),
    % TODO: get version if ok
    ?LOG_INFO("[PEER] Started ~s", [Name]),
    {ok, active, #data{ip = Ip, port = Port, name = Name}, [
        {next_event, internal, compare_latest}
    ]}.

callback_mode() ->
    state_functions.

active(state_timeout, compare_latest, _D) ->
    Action = {next_event, internal, compare_latest},
    {keep_state_and_data, [Action]};

active(internal, compare_latest, D) ->
    {ok, Config} = coin:env([http]),
    R = gun_post(D, "/api/peer", #{
        ip => maps:get(<<"ip">>, Config),
        port => maps:get(<<"port">>, Config)
    }),
    ?LOG_INFO("[PEER] POST /api/peer result ~p", [R]),

    case gun_get(D, "/api/blockchain/latest") of
        {200, Json} ->
            {ok, RemoteBlock} = coin_proto:decode_block_json(Json),
            {ok, LocalBlock} = coin_chain:latest(),
            compare_latest(LocalBlock, RemoteBlock, D);
        {Status, _} when is_integer(Status) ->
            ?LOG_WARNING("[PEER] unable to connect to ~s status ~p", [D#data.name, Status]),
            {stop, normal};
        {error, timeout} ->
            ?LOG_WARNING("[PEER] unable to connect to ~s reason timeout", [D#data.name]),
            {keep_state_and_data, [{state_timeout, ?POLL_TIMEOUT, compare_latest}]};
        {error, Reason} ->
            ?LOG_WARNING("[PEER] unable to connect to ~s reason ~p", [D#data.name, Reason]),
            {stop, normal}
    end;

active(cast, {send_block, Block}, D) ->
    ?LOG_INFO("[PEER] sending ~s block ~s", [D#data.name, coin_util:brief(Block#block.hash, 8)]),
    {ok, Json} = coin_proto:encode_block_json(Block),
    R = gun_post(D, "/api/blockchain", Json),
    ?LOG_INFO("[PEER] POST ~s /api/blockchain result ~p", [D#data.name, R]),
    {keep_state_and_data, []};

active(Type, Content, D) ->
    handle_common(Type, Content, ?FUNCTION_NAME, D).

handle_common(Type, Content, State, D) ->
    ?LOG_WARNING("[PEER] unhandled event ~p ~p ~p ~p", [State, Type, Content, D]),
    {keep_state_and_data, []}.

terminate(_Reason, _State, _D = #data{}) ->
    ok.

code_change(_OldVsn, State, D = #data{}, _Extra) ->
    {ok, State, D}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_name(Ip, Port) ->
    list_to_binary(inet:ntoa(Ip) ++ ":" ++ integer_to_list(Port)).

check_not_self(Ip, Port) ->
    {ok, SelfIpStr} = coin:env([http, ip]),
    {ok, SelfIp} = inet:parse_strict_address(binary_to_list(SelfIpStr)),
    {ok, SelfPort} = coin:env([http, port]),
    if
        Ip == SelfIp andalso Port == SelfPort ->
            error(badarg, [Ip, Port]);
        true ->
            ok
    end.

compare_latest(#block{index = Idx}, #block{index = RIdx}, D) when RIdx > Idx ->
    ?LOG_DEBUG("[PEER] ~s blockchain out of sync we have ~w while remote has ~w", [D#data.name, Idx, RIdx]),
    {200, Json} = gun_get(D, "/api/blockchain/~w", [Idx + 1]),
    {ok, Block} = coin_proto:decode_block_json(Json),
    Res = coin_chain:append(Block),
    ?LOG_INFO("[PEER] append result ~p", [Res]),
    {keep_state_and_data, [{state_timeout, ?POLL_TIMEOUT, compare_latest}]};
compare_latest(_, _, D) ->
    ?LOG_INFO("[PEER] ~s blockchain synched", [D#data.name]),
    {keep_state_and_data, [{state_timeout, ?POLL_TIMEOUT, compare_latest}]}.

%%%===================================================================
%%% HTTP CLIENT
%%%===================================================================

headers() ->
    [
        {<<"user-agent">>, "coin"},
        {<<"accept">>, <<"application/json">>}
    ].

gun_get(D, Fmt, Args) ->
    gun_get(D, iolist_to_binary(io_lib:format(Fmt, Args))).

gun_get(#data{ip = Ip, port = Port}, Path) ->
    case gun_open(Ip, Port) of
        Pid when is_pid(Pid) ->
            StreamRef = gun:get(Pid, Path, headers()),
            gun_wait(Pid, StreamRef);
        {error, _} = Err ->
            Err
    end.

gun_post(D, Path, Payload) when is_map(Payload) ->
    gun_post(D, Path, jsx:encode(Payload));
gun_post(#data{ip = Ip, port = Port}, Path, Payload) when is_binary(Payload) ->
    case gun_open(Ip, Port) of
        Pid when is_pid(Pid) ->
            StreamRef = gun:post(Pid, Path, headers(), Payload),
            gun_wait(Pid, StreamRef);
        {error, _} = Err ->
            Err
    end.

gun_open(Ip, Port) ->
    {ok, Pid} = gun:open(Ip, Port, #{protocols => [http2]}),
    case gun:await_up(Pid) of
        {ok, _} ->
            Pid;
        {error, Reason} ->
            gun:close(Pid),
            {error, Reason}
    end.

gun_wait(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, _Headers} ->
            gun:close(ConnPid),
            {Status, no_data};
        {response, nofin, Status, _Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            gun:close(ConnPid),
            {Status, Body};
        {error, Reason} ->
            gun:close(ConnPid),
            {error, Reason}
    end.

%%%===================================================================
%%% HTTP SERVER
%%%===================================================================

handle_http(<<"GET">>, <<"/api/peer">>, _Req, _Data) ->
    L = lists:map(fun to_json/1, coin_reg:peers()),
    {200, coin_http:headers(), L};

handle_http(<<"POST">>, <<"/api/peer">>, _Req, Data) ->
    Opts = from_json(Data),
    case coin_peer_sup:start_child(Opts) of
        {ok, _} ->
            {200, coin_http:headers(), #{result => <<"ok">>}};
        {error, {already_started, _}} ->
            {200, coin_http:headers(), #{result => <<"ok">>}};
        {error, Reason} ->
            ?LOG_WARNING("[PEER] unabled to start peer ~p reason ~p", [Opts, Reason]),
            {400, coin_http:headers(), #{result => <<"error">>}}
    end.

to_json({Ip, Port}) ->
    #{
        ip => list_to_binary(inet:ntoa(Ip)),
        port => Port
    }.

from_json(#{ip := IpStr, port := Port}) ->
    {ok, Ip} = inet:parse_strict_address(binary_to_list(IpStr)),
    [{ip, Ip}, {port, Port}].