-module(coin_base58).
-author("pfav").

%% API
-export([
    encode/1,
    decode/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.


% https://tools.ietf.org/id/draft-msporny-base58-01.html
% https://en.bitcoin.it/wiki/Base58Check_encoding


% encode/1 --------------------------------------------------------------------

-spec encode(binary()) ->
    binary().

encode(Bin) ->
    iolist_to_binary(encode_zero(Bin)).

encode_zero(<<>>) ->
    [];
encode_zero(<<0, Rest/binary>>) ->
    [$1 | encode_zero(Rest)];
encode_zero(Bin) ->
    encode_from_integer(binary:decode_unsigned(Bin), []).

encode_from_integer(0, Acc) ->
    Acc;
encode_from_integer(N, Acc) ->
    encode_from_integer(N div 58, [b58e(N rem 58) | Acc]).


% decode/1 --------------------------------------------------------------------

-spec decode(binary()) ->
    binary().

decode(Bin) ->
    iolist_to_binary(decode_zero(Bin)).

decode_zero(<<>>) ->
    [];
decode_zero(<<$1, R/binary>>) ->
    [ 0 | decode_zero(R) ];
decode_zero(Bin) ->
    [ binary:encode_unsigned(decode_to_integer(Bin)) ].

decode_to_integer(<<Ch>>) ->
    b58d(Ch);
decode_to_integer(<<C, R/binary>>) ->
    decode_to_integer(b58d(C), R).

decode_to_integer(Carry, <<>>) ->
    Carry;
decode_to_integer(Carry, <<Ch, Rest/binary>>) ->
    case b58d(Ch) of
        N when is_integer(N) ->
            decode_to_integer(Carry * 58 + N, Rest);
        bad ->
            error(badarg, [Ch])
    end.


-compile({inline, [{b58d, 1}]}).
b58d(X) ->
    element(X, {
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,0,1,2,3,4,5,6,7,8,bad,bad,bad,
        bad,bad,bad,bad,9,10,11,12,13,14,15,16,bad,17,18,19,20,21,bad,22,
        23,24,25,26,27,28,29,30,31,32,bad,bad,bad,bad,bad,bad,33,34,35,36,
        37,38,39,40,41,42,43,bad,44,45,46,47,48,49,50,51,52,53,54,55,
        56,57,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,
        bad,bad,bad,bad,bad,bad,bad,bad,bad,bad}).

-compile({inline, [{b58e, 1}]}).
b58e(X) ->
    element(X+1, {
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E,
        $F, $G, $H, $J, $K, $L, $M, $N, $P, $Q, $R, $S, $T, $U,
        $V, $W, $X, $Y, $Z, $a, $b, $c, $d, $e, $f, $g, $h, $i,
        $j, $k, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x,
        $y, $z
    }).


-ifdef(TEST).

decode_test() ->
    % as defined on paper
    ?assertEqual(<<"Hello World!">>, decode(<<"2NEpo7TZRRrLZSi2U">>)),
    ?assertEqual(<<"The quick brown fox jumps over the lazy dog.">>, decode(<<"USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z">>)),
    ?assertEqual(16#0000287fb4cd, binary:decode_unsigned(decode(<<"111233QC4">>))),
    ok.

encode_test() ->
    % as defined on paper
    ?assertEqual(<<"2NEpo7TZRRrLZSi2U">>, encode(<<"Hello World!">>)),
    ?assertEqual(<<"USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z">>, encode(<<"The quick brown fox jumps over the lazy dog.">>)),
    ?assertEqual(<<"111233QC4">>, encode(<<16#00, 16#00, 16#00, 16#28, 16#7f, 16#b4, 16#cd>>)),
    ok.

b58ed_test() ->
    CheckFn = fun (I) -> ?assertEqual(I, b58d(b58e(I))) end,
    lists:foreach(CheckFn, lists:seq(0, 57)),
    ok.


-endif.