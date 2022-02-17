-module(coin_http).
-author("pfav").


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start/0,
    init/2
]).

%% helpers
-export([
    headers/0,
    error404/0,
    error505/0,
    constraint_branch_slot/2,
    constraint_non_neg_integer/2,
    constraint_pos_integer/2,
    constraint_valid_hash/2,
    constraint_valid_wallet_address/2
]).

%% DEBUG
-export([
    routes_dump/1
]).

%% start/0 --------------------------------------------------------------------

-spec start() ->
    ok | no_return().

start() ->
    Opts = opts(),
    Routes = api_list() ++ static_list(),
    % routes_dump(Routes),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    persistent_term:put(?MODULE, Dispatch),
    case cowboy:start_clear(?MODULE, Opts, #{env => #{dispatch => {persistent_term, ?MODULE}}}) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

opts() ->
    {ok, Config} = coin:env([http]),
    {ok, Ip} = inet:parse_strict_address(binary_to_list(maps:get(<<"ip">>, Config))),
    Port = maps:get(<<"port">>, Config),

    ?LOG_NOTICE("[HTTP] Listening ~s:~w", [inet:ntoa(Ip), Port]),
    #{
        socket_opts => [
            {ip, Ip},
            {port, Port}
        ],
        num_acceptors => maps:get(<<"acceptor">>, Config, 4)
    }.

%% ROUTES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

routes_dump(L) ->
    lists:foreach(fun route_dump/1, L).

route_dump({Path, Constr, Mod, {_, M}}) ->
    Methods = maps:keys(M),
    lists:foreach(fun (Method) ->
        erlang:display({Method, Constr, Path, Mod})
    end, Methods);
route_dump({Path, Mod, Arg}) ->
    erlang:display({Path, Mod, Arg}).

static_list() ->
    [
        static_file("/", "www/index.html"),
        static_dir("/[...]", "www")
    ].

api_list() ->
    Slot = {slot, [int, fun constraint_branch_slot/2]},
    Hash = {hash, [fun constraint_valid_hash/2]},
    Index = {index, [int, fun constraint_non_neg_integer/2]},

    compile_api([
        % application
        {<<"GET">>, <<"/api/version">>, fun handle_http/4},

        % blockchain
        {<<"GET">>,        <<"/api/blockchain/info">>,         fun coin_chain:handle_http/4},

        {<<"GET">>, [Slot], <<"/api/blockchain/:slot/info">>,   fun coin_chain:handle_http/4},
        {<<"GET">>, [Slot], <<"/api/blockchain/:slot/latest">>, fun coin_chain:handle_http/4},

        {<<"GET">>,            <<"/api/blockchain">>,              fun coin_chain:handle_http/4},
        {<<"POST">>,           <<"/api/blockchain">>,              fun coin_chain:handle_http/4},

        {<<"GET">>,  [Hash],   <<"/api/blockchain/:hash">>,        fun coin_chain:handle_http/4},
        {<<"GET">>,  [Index], <<"/api/blockchain/:index">>,      fun coin_chain:handle_http/4},
        {<<"GET">>,  [Slot],   <<"/api/blockchain/latest">>,       fun coin_chain:handle_http/4},

        % peer
        {<<"GET">>, <<"/api/peer">>, fun coin_peer:handle_http/4},
        {<<"POST">>, <<"/api/peer">>, fun coin_peer:handle_http/4},

        % wallet
        {<<"GET">>,  [Hash], <<"/api/wallet/:hash">>,      fun coin_wallet:handle_http/4},
        {<<"POST">>, [Hash], <<"/api/wallet/:hash/pay">>,  fun coin_wallet:handle_http/4},
        {<<"GET">>,  [Hash], <<"/api/wallet/:hash/utxo">>,  fun coin_wallet:handle_http/4}
    ]) ++ [
        {<<"/api/[...]">>, ?MODULE, not_found}
    ].


%% HANDLER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_http(<<"GET">>, <<"/api/version">>, _Req, _Data) ->
    {ok, Name} = coin:env([coin, name]),
    Result = #{
        version => coin_util:version(),
        name => Name
    },
    {200, headers(), Result}.


%% MAIN_HANDLER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req0, {HandlerPath, Routes}) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    case maps:find(Method, Routes) of
        {ok, Handler} ->
            try
                {Req1, Body} = read_http_body(Req0),
                Bindings = cowboy_req:bindings(Req0),
                timer:tc(fun () -> Handler(Method, HandlerPath, Req1, maps:merge(Bindings, Body)) end)
            of
                {Ms, {Status, Headers, Payload}} ->
                    ?LOG_NOTICE("[HTTP] ~s ~s ~w ~.2fms", [Method, Path, Status, Ms / 1000]),
                    Response = format_response(Payload),
                    Req2 = cowboy_req:reply(Status, Headers, Response, Req1),
                    {ok, Req2, undefined}
            catch
                exit:{request_error, {match_qs, Err}, 'Query string validation constraints failed for the reasons provided.'}:_ ->
                    ?LOG_ERROR("[HTTP] ~s ~s: ~p", [Method, Path, Err]),
                    handle_error(Req0, 400, <<"Bad Request">>);
                K:E:S ->
                ?LOG_ERROR("[HTTP] ~s ~s: ~p ~p ~p", [Method, Path, K, E, S]),
                handle_error(Req0, 505, <<"Internal Server Error">>)
            end;
        error ->
            handle_error(Req0, 405, <<"Method Not Allowed">>)
    end;
init(Req0, not_found) ->
    handle_error(Req0, 404, <<"Not Found">>).

read_http_body(Req0) ->
    case cowboy_req:has_body(Req0) of
       true ->
           {ok, Body, Req1} = cowboy_req:read_body(Req0),
           Payload = jsx:decode(Body, [{labels, existing_atom}]),
           {Req1, Payload};
        false ->
            {Req0, #{}}
    end.

format_response(Payload) when is_binary(Payload) ->
    Payload;
format_response(no_data) ->
    <<>>;
format_response(Payload) ->
    jsx:encode(Payload).

headers() ->
    #{<<"content-type">> => <<"application/json">>}.

error404() ->
    {404, headers(), #{result => <<"Not Found">>}}.

error505() ->
    {505, headers(), #{result => <<"Internal Server Error">>}}.

handle_error(Req0, Status, Result) ->
    Req1 = cowboy_req:reply(Status, headers(), jsx:encode(#{result => error, message => Result}), Req0),
    {ok, Req1, undefined}.



%% util -----------------------------------------------------------------------

compile_api(L) ->
    compile_api(L, []).

compile_api([], Acc) ->
    [ {Path, Constraints, ?MODULE, {Path, Route}} || {Path, Constraints, Route} <- lists:reverse(Acc)];

compile_api([{Method, Path, Handler} | R], Acc0) ->
    Acc1 = add_path(Method, Path, Handler, [], Acc0),
    compile_api(R, Acc1);
compile_api([{Method, Constraints, Path, Handler} | R], Acc0) ->
    Acc1 = add_path(Method, Path, Handler, Constraints, Acc0),
    compile_api(R, Acc1).

add_path(Method, Path, Handler, Constraints, Acc0) ->
    case lists:keyfind(Path, 1, Acc0) of
        false ->
            M = maps:put(Method, Handler, #{}),
            [{Path, Constraints, M} | Acc0];
        {Path, Constraints, M0} ->
            M1 = maps:put(Method, Handler, M0),
            lists:keyreplace(Path, 1, Acc0, {Path, Constraints, M1});
        PathTuple ->
            error(badarg, [PathTuple])
    end.

static_file(HttpPath, PrivPath) ->
    {HttpPath, cowboy_static, {priv_file, coin_util:app(), PrivPath}}.
static_dir(HttpPath, PrivPath) ->
    {HttpPath, cowboy_static, {priv_dir, coin_util:app(), PrivPath}}.

%% constraints

constraint_branch_slot(forward, Value) when Value >= 0 andalso Value < 128 ->
    {ok, Value};
constraint_branch_slot(forward, _Value) ->
    {error, invalid_branch}.

constraint_non_neg_integer(forward, Value) when Value >= 0 ->
    {ok, Value};
constraint_non_neg_integer(forward, _Value) ->
    {error, invalid_non_neg_integer}.

constraint_pos_integer(forward, Value) when Value > 0 ->
    {ok, Value};
constraint_pos_integer(forward, _Value) ->
    {error, invalid_pos_integer}.

constraint_valid_hash(forward, Hash) when size(Hash) == 128 -> % 64Byte hex encoded
    {ok, binary:decode_hex(Hash)};
constraint_valid_hash(forward, _Hash) ->
    {error, invalid_hash}.

constraint_valid_wallet_address(forward, Address) when size(Address) == 64 -> % 32Byte hex encoded
    {ok, binary:decode_hex(Address)};
constraint_valid_wallet_address(forward, _Address) ->
    {error, invalid_wallet_address}.