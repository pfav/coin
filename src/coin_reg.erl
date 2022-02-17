-module(coin_reg).
-author("pfav").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

%% API
-export([
    peer_key/2,
    peers_pids/0,
    peers/0
]).

%% DEBUG
-export([
    dump/0
]).

%% PEER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PEER_KEY(Ip, Port), {coin_peer, Ip, Port}).

peer_key(Ip, Port) ->
    {via, gproc, {n, l, ?PEER_KEY(Ip, Port)}}.

peers_pids() ->
    gproc:lookup_pids({n, l, ?PEER_KEY('_', '_')}).

peers() ->
    gproc:select([{
        {{n, l, ?PEER_KEY('$1', '$2')}, '_', '_'},
        [],
        [{{'$1', '$2'}}]
    }]).

dump() ->
    gproc:select([{'_', [], ['$_']}]).
