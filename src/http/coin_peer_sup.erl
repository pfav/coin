-module(coin_peer_sup).
-author("pfav").

-behaviour(supervisor).

-export([
    start_link/0,
    init/1,
    start_child/1,
    stop_child/1,
    pids/0,
    count/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Opts) ->
    supervisor:start_child(?MODULE, [Opts]).

stop_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

pids() ->
    lists:map(fun (E) -> element(2, E) end, supervisor:which_children(?MODULE)).

count() ->
    proplists:get_value(workers, supervisor:count_children(?MODULE)).


init([]) ->
    Cfg = #{
        strategy => simple_one_for_one,
        intensity => 30,
        period => 5
    },
    Child = coin_peer_spec(),
    {ok, {Cfg, [Child]}}.

coin_peer_spec() ->
    #{
        id => coin_peer,
        start => {coin_peer, start_link, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => [coin_peer]
    }.
