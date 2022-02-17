%%%-------------------------------------------------------------------
%% @doc coin top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(coin_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
    ]).

-define(TABLES, [
    {coin_mempool,     [set, {keypos, 2}]}
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    coin_db:init(),
    init_tab(?TABLES),

    Cfg = #{
        strategy => one_for_all,
        intensity => 60,
        period => 5
    },

    Children = [
        coin_chain_spec(),
        coin_peer_sup_spec(),
        coin_peer_mon_spec()
    ],

    {ok, {Cfg, Children}}.

%% internal functions

coin_chain_spec() ->
    #{
        id => coin_chain,
        start => {coin_chain, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [coin_chain]
    }.

coin_peer_mon_spec() ->
    #{
        id => coin_peer_mon,
        start => {coin_peer_mon, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [coin_peer_mon]
    }.

coin_peer_sup_spec() ->
    #{
        id => coin_peer_sup,
        start => {coin_peer_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [coin_peer_sup]
    }.


%% ets_new/1

init_tab(List)
    when is_list(List) ->
    lists:foreach(fun init_tab/1, List);

init_tab({Table, Opts}) ->
    ets:new(Table, [named_table, public | Opts]).
