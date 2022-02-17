-module(coin_address).
-author("pfav").

-include_lib("coin/include/coin.hrl").

%% API
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-export([
    encode_public/1,
    encode_private/1,
    decode_private/1,
    verify/1,
    version/1
]).

-export([
    i/1
]).

-define(VERSION_PUBLIC, 0).
-define(VERSION_PRIVATE, 128).


%% encode_public/1 ------------------------------------------------------------

-spec encode_public(PubKey) -> Address when
    PubKey :: coin_key:public_key(),
    Address :: binary().

encode_public(PubKey) ->
    Hash = crypto:hash(ripemd160, crypto:hash(blake2b, PubKey)),
    TargetHash = <<?VERSION_PUBLIC, Hash/binary>>,
    Checksum = checksum(TargetHash),
    coin_base58:encode(<<TargetHash/binary, Checksum/binary>>).

%% encode_private/1 -----------------------------------------------------------

-spec encode_private(PrivKey) -> Address when
    PrivKey :: coin_key:private_key(),
    Address :: binary().

encode_private(PrivKey) ->
    TargetKey = <<?VERSION_PRIVATE, PrivKey/binary>>,
    Checksum = checksum(TargetKey),
    coin_base58:encode(<<TargetKey/binary, Checksum/binary>>).

%% decode_private/1 -----------------------------------------------------------

-spec decode_private(Address) -> {ok, PrivKey} | {error, invalid} when
    Address :: binary(),
    PrivKey :: coin_key:private_key().

decode_private(Address) ->
    case coin_base58:decode(Address) of
        <<?VERSION_PRIVATE, Bin/binary>> ->
            {ok, strip_checksum(Bin)};
        _ ->
            {error, invalid}
    end.

%% verify/1 -------------------------------------------------------------------

-spec verify(Address) -> boolean() when
    Address :: binary().

verify(Address) ->
    Bin = coin_base58:decode(Address),
    get_checksum(Bin) == checksum(strip_checksum(Bin)).


%% version/1 ------------------------------------------------------------------

-spec version(Address) -> ?VERSION_PRIVATE | ?VERSION_PUBLIC when
    Address :: binary().

version(Address) ->
    <<Vsn, _/binary>> = coin_base58:decode(Address),
    Vsn.

i(#keypair{pub = Pub, priv = Priv}) ->
    Public = encode_public(Pub),
    Private = encode_private(Priv),
    io:format("== address ==~n"),
    io:format("sk: ~s ~w ~p~n", [Private, size(Private), verify(Private)]),
    io:format("pk: ~s ~w ~p~n", [Public, size(Public), verify(Public)]),
    ok.

%% ============================================================================

checksum(Bin) ->
    binary_part(
        crypto:hash(blake2b, crypto:hash(blake2b, Bin)),
        {0, 4}
    ).

strip_checksum(Bin) ->
    binary_part(Bin, {0, size(Bin)-4}).

get_checksum(Bin) ->
    binary_part(Bin, {size(Bin), -4}).

-ifdef(TEST).

public_address_test() ->
    K = crypto:strong_rand_bytes(32),
    A = encode_public(K),
    ?assertEqual(true, verify(A)),
    ?assertEqual(?VERSION_PUBLIC, version(A)),
    ok.

private_address_test() ->
    K = crypto:strong_rand_bytes(32),
    A = encode_private(K),
    ?assertEqual(true, verify(A)),
    ?assertEqual(?VERSION_PRIVATE, version(A)),
    ?assertEqual({ok, K}, decode_private(A)),
    ?assertEqual({error, invalid}, decode_private(encode_public(K))),
    ok.


-endif.
