-module(coin_util).
-author("pfav").

%% API
-export([
    app/0,
    version/0,
    priv/0,
    brief/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.



% app/0 -----------------------------------------------------------------------

-spec app() -> atom().

app() ->
    {ok, App} = application:get_application(?MODULE),
    App.

% version/0 -------------------------------------------------------------------

-spec version() -> binary().

version() ->
    {ok, Vsn} = application:get_key(app(), vsn),
    list_to_binary(Vsn).


% priv/0 ----------------------------------------------------------------------

-spec priv() ->
    string().

priv() ->
    case code:priv_dir(app()) of
        Dir when is_list(Dir) ->
            Dir;
        {error, bad_name} ->
            error(bad_name)
    end.

% brief/2 ---------------------------------------------------------------------

-spec brief(Bin, N) -> Result when
    Bin :: binary(),
    N :: non_neg_integer(),
    Result :: binary().

brief(Bin, N) when N > 0, size(Bin) >= N * 2 ->
    iolist_to_binary([
        binary:encode_hex(binary_part(Bin, {0, N})),
        "..",
        binary:encode_hex(binary_part(Bin, {size(Bin), -N}))
    ]);
brief(Hash, N) ->
    error(badarg, [Hash, N]).


-ifdef(TEST).

brief_test() ->
    ?assertEqual(<<"0A..0A">>, brief(<<10,10,10,10>>, 1)),
    ?assertEqual(<<"0A0A..0A0A">>, brief(<<10,10,10,10>>, 2)),
    ?assertError(badarg, brief(<<10,10,10,10>>, 3)),
    ?assertError(badarg, brief(<<10,10,10,10>>, 4)),
    ?assertError(badarg, brief(<<>>, 0)),
    ?assertError(badarg, brief(<<>>, 1)),
    ok.

-endif.