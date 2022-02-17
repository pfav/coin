-module(coin_branch).
-author("pfav").


%% API
-export([
    insert/2,
    lookup/1,
    append/1,
    remove/1,
    find/1,
    all/0,
    best/0
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("kernel/include/logger.hrl").
-include_lib("coin/include/coin.hrl").

-type slot() :: 0..255.
-type branch() :: #branch{}.

-export_type([slot/0, branch/0]).

%% This module handles the different branches the chain will have.
%% A maximum of 256 branches will be stored.
%% Each branch is identified by an index and once a new block is inserted
%% the `tip` hash is updated accordingly.
%%
%% [abc]      <- branch(0)
%%
%% after append of block xyz and qwr
%%
%%      [xyz] <- branch(0)
%%     /
%% [abc]
%%     \
%%      [qwr] <- branch(1)
%%
%% branch(0) will point to xyz hash and
%% branch(1) will point to qwr hash.

-define(IS_VALID_SLOT(S), (is_integer(S) andalso (S >= 0 andalso S < 256))).

%% lookup/1 -------------------------------------------------------------------

-spec lookup(Slot) -> {ok, Branch} | {error, any()} when
    Slot :: slot(),
    Branch :: branch().

lookup(Slot) when ?IS_VALID_SLOT(Slot) ->
    case coin_db:lookup(?BRANCH_KEY(Slot)) of
        {ok, Bin} ->
            coin_proto:decode_branch(Bin);
        {error, Reason} ->
            {error, Reason}
    end;
lookup(Branch) ->
    error(badarg, [Branch]).

%% insert/2 -------------------------------------------------------------------

-spec insert(Slot, Branch) -> ok | {error, any()} when
    Slot :: slot(),
    Branch :: branch().

insert(Slot, Branch)
    when ?IS_VALID_SLOT(Slot)
    ->
    {ok, Bin} = coin_proto:encode_branch(Branch),
    coin_db:insert(?BRANCH_KEY(Slot), Bin);
insert(Slot, Branch) ->
    error(badarg, [Slot, Branch]).

%% append/1 -------------------------------------------------------------------

-spec append(Branch) -> ok | {error, any()} when
    Branch :: branch().

append(#branch{} = Branch) ->
    append(Branch, 0);
append(Branch) ->
    error(badarg, [Branch]).

append(_, 256) ->
    {error, full};
append(#branch{latest_hash = Hash} = Branch, Slot) ->
    case lookup(Slot) of
        {ok, #branch{latest_hash = Hash}} ->
            {error, already_present};
        {ok, _} ->
            append(Branch, Slot+1);
        {error, not_found} ->
            insert(Slot, Branch);
        {error, Reason} ->
            {error, Reason}
    end.

%% remove/1 -------------------------------------------------------------------

-spec remove(Slot) -> ok | {error, any()} when
    Slot :: slot().

remove(Slot) when ?IS_VALID_SLOT(Slot) ->
    coin_db:delete(?BRANCH_KEY(Slot));
remove(Slot) ->
    error(badarg, [Slot]).


%% find/1 ---------------------------------------------------------------------

-spec find(Hash) -> {ok, Slot} | {error, any()} when
    Hash :: coin_block:hash(),
    Slot :: slot().

find(Hash) when is_binary(Hash) ->
    case coin_db:each(fun ({<<Slot>>, Bin}) ->
        case coin_proto:decode_branch(Bin) of
            {ok, #branch{latest_hash = Hash}} ->
                {stop, Slot};
            _ ->
                continue
        end
                      end, ?BRANCH_PREFIX)
    of
        {ok, Slot} ->
            {ok, Slot};
        ok ->
            {error, not_found}
    end;
find(Hash) ->
    error(badarg, [Hash]).

%% all/0 ----------------------------------------------------------------------

-spec all() -> {ok, Result} | {error, any()} when
    Result :: [{slot(), branch()}].

all() ->
    {ok, lists:reverse(
        coin_db:fold(
            fun ({<<Slot>>, Bin}, Acc) ->
                {ok, Branch} = coin_proto:decode_branch(Bin),
                [{Slot, Branch} | Acc]
            end, [], ?BRANCH_PREFIX))}.


%% best/0 ---------------------------------------------------------------------

-spec best() ->
    {ok, slot()} | {error, any()}.

best() ->
    case all() of
        {ok, [{Slot, #branch{cumulative_difficulty = Diff}} | R]} ->
            best(R, {Slot, Diff});
        {ok, []} ->
            {error, invalid}
    end.

best([], {Slot, _}) ->
    {ok, Slot};
best([{Slot, #branch{cumulative_difficulty = Diff1}} | R], {_, Diff0})
    when Diff1 > Diff0 ->
    best(R, {Slot, Diff1});
best([_ | R], Acc) ->
    best(R, Acc).


-ifdef(TEST).
-endif.


