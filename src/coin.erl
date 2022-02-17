-module(coin).
-author("pfav").

-include_lib("coin/include/coin.hrl").
-include("./proto/CoinProto.hrl").

%% API
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-export([
    env/0, env/1, env/2
]).

env() ->
    coin_env:get_config().

env(K) ->
    coin_env:get_config(K).

env(K, Default) ->
    case coin_env:get_config(K) of
        {ok, V} ->
            V;
        {error, not_found} ->
            Default
    end.