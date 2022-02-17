%%%-------------------------------------------------------------------
%% @doc coin public API
%% @end
%%%-------------------------------------------------------------------

-module(coin_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    coin_env:init(),
    coin_http:start(),
    coin_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
