-module(coin_peer_mon).
-author("pfav").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([
    init/1,
    active/3,
    terminate/3,
    code_change/4,
    callback_mode/0]).

-ifndef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("kernel/include/logger.hrl").

-record(data, {
    tab :: ets:tid()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
    ?LOG_INFO("[PEER_MON] Started"),
    Tab = ets:new(?MODULE, []),
    case coin:env([peer]) of
        {ok, PeerConfig} ->
            lists:foreach(fun (Peer) -> start_peer(Tab, Peer) end, PeerConfig),
            {ok, active, #data{tab = Tab}};
        _ ->
            {ok, active, #data{tab = Tab}}
    end.

callback_mode() ->
    state_functions.

active(info, {'DOWN', Ref, process, _Pid, _Reason}, #data{tab = Tab}) ->
    case ets:lookup(Tab, Ref) of
        [{_, Json}] ->
            ets:delete(Tab, Ref),
            start_peer(Tab, Json),
            {keep_state_and_data, []};
        _ ->
            ets:delete(Tab, Ref),
            {keep_state_and_data, []}
    end;

active(Type, Content, D = #data{}) ->
    handle_common(Type, Content, ?FUNCTION_NAME, D).

handle_common(Type, Content, State, D = #data{}) ->
    ?LOG_WARNING("[PEER_MON] unhandled message ~p ~p ~p ~p", [Type, Content, State, D]),
    {keep_state_and_data, []}.

terminate(_Reason, _State, _D = #data{}) ->
    ok.
code_change(_OldVsn, State, D = #data{}, _Extra) ->
    {ok, State, D}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_peer(Tab, #{<<"ip">> := IpStr, <<"port">> := Port} = Json) ->
    {ok, Ip} = inet:parse_strict_address(binary_to_list(IpStr)),
    ?LOG_NOTICE("[PEER_MON] starting ~s:~w", [IpStr, Port]),
    case coin_peer_sup:start_child([{ip, Ip}, {port, Port}]) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            ets:insert(Tab, {Ref, Json});
        {error, {already_started, Pid}} ->
            Ref = erlang:monitor(process, Pid),
            ets:insert(Tab, {Ref, Json});
        {error, Reason} ->
            ?LOG_WARNING("[PEER_MON] failed to start peer ~s:~w reason ~p", [IpStr, Port, Reason])
    end.