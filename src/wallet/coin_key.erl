-module(coin_key).
-author("pfav").

%% API
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

-include_lib("coin/include/coin.hrl").

-export([
    new_keypair/0,
    new_keypair/1,

    pub/0,
    priv/0,

    get_key/0,
    set_key/1,
    is_valid/1,

    sign/2,
    verify/3,

    filename/0,
    to_json/1,
    i/0
]).

-define(CURVE_NAME, eddsa).
-define(CURVE_PARAM, ed25519).
-define(CURVE_SIGN_HASH, sha512).
-define(PUBLIC_KEY_SIZE, 32).
-define(PRIVATE_KEY_SIZE, 32).

-type private_key() :: <<_:256>>.
-type public_key() :: <<_:256>>.

-export_type([private_key/0, public_key/0]).

%% new_keypair/0 --------------------------------------------------------------

-spec new_keypair() ->
    #keypair{}.

new_keypair() ->
    {Pub, Priv} = crypto:generate_key(?CURVE_NAME, ?CURVE_PARAM),
    #keypair{pub = Pub, priv = Priv}.

%% new_keypair/1 --------------------------------------------------------------

-spec new_keypair(PrivKey) ->
    #keypair{} when
    PrivKey :: private_key().

new_keypair(PrivKey) when size(PrivKey) == ?PRIVATE_KEY_SIZE ->
    {Pub, Priv} = crypto:generate_key(?CURVE_NAME, ?CURVE_PARAM, PrivKey),
    #keypair{pub = Pub, priv = Priv};
new_keypair(Bin) ->
    error(badarg, [Bin]).

% pub/0 -----------------------------------------------------------------------

-spec pub() ->
    public_key().

pub() ->
    (?MODULE:get_key())#keypair.pub.

% priv/0 ----------------------------------------------------------------------

-spec priv() ->
    private_key().

priv() ->
    (?MODULE:get_key())#keypair.priv.

% get_key/0 -------------------------------------------------------------------

-spec get_key() ->
    #keypair{} | no_return().

get_key() ->
    case get_cache() of
        KeyPair when is_record(KeyPair, 'keypair') ->
            KeyPair;
        _ ->
            case get_file() of
                {ok, PrivateKey} ->
                    set_cache(KeyPair = new_keypair(PrivateKey)),
                    KeyPair;
                {error, enoent} ->
                    set_cache(KeyPair = new_keypair()),
                    ok = set_file(KeyPair),
                    KeyPair;
                {error, Reason} ->
                    error(Reason)
            end
    end.

%% set_key/1 ------------------------------------------------------------------

-spec set_key(PrivateKey) -> ok | {error, term()} when
    PrivateKey :: private_key().

set_key(PrivateKey) when size(PrivateKey) == ?PUBLIC_KEY_SIZE ->
    KeyPair = new_keypair(PrivateKey),
    case is_valid(KeyPair) of
        true ->
            set_cache(KeyPair);
        false ->
            {error, bad_key}
    end.

%% is_valid/1 -----------------------------------------------------------------

-spec is_valid(KeyPair) -> boolean() when
    KeyPair :: #keypair{}.

is_valid(#keypair{pub = Pub, priv = Priv})
    when
    is_binary(Pub), size(Pub) == ?PUBLIC_KEY_SIZE,
    is_binary(Priv), size(Priv) == ?PUBLIC_KEY_SIZE
    ->
    true;
is_valid(_) ->
    false.

%% sign/2 ---------------------------------------------------------------------

-spec sign(Data, KeyPair) -> Signature when
    Data :: iolist(),
    KeyPair :: #keypair{} | private_key(),
    Signature :: binary().

sign(Data, #keypair{priv = Priv}) ->
    sign(Data, Priv);
sign(Data, Priv) when is_binary(Priv), size(Priv) == ?PRIVATE_KEY_SIZE ->
    crypto:sign(?CURVE_NAME, ?CURVE_SIGN_HASH, Data, [Priv, ?CURVE_PARAM]).

%% verify/3 -------------------------------------------------------------------

-spec verify(Data, Signature, PubKey) -> boolean() when
    Data :: iolist(),
    Signature :: binary(),
    PubKey :: public_key().

verify(Data, Signature, PubKey) when size(PubKey) == ?PUBLIC_KEY_SIZE ->
    crypto:verify(?CURVE_NAME, ?CURVE_SIGN_HASH, Data, Signature, [PubKey, ?CURVE_PARAM]).

%% to_json/1 ------------------------------------------------------------------

-spec to_json(Keypair) -> Json when
    Keypair :: #keypair{},
    Json :: map().

to_json(#keypair{pub = PubKey, priv = PrivKey}) ->
    #{
        public_key => binary:encode_hex(PubKey),
        private_key => coin_address:encode_private(binary:encode_hex(PrivKey))
    }.

%% filename/0 -----------------------------------------------------------------

-spec filename() -> binary() | no_return().

filename() ->
    case coin:env([wallet, path]) of
        {ok, Filename} ->
            Filename;
        {error, not_found} ->
            error(missing_wallet_path)
    end.

%% i/0 ------------------------------------------------------------------------

i() ->
    {ok, Path} = coin:env([wallet, path]),
    KeyPair = get_key(),
    io:format("== keypair ==~n"),
    io:format("keydb: ~s~n", [Path]),
    io:format("sk: ~s~n", [coin_address:encode_private(KeyPair#keypair.priv)]),
    io:format("pk: ~s~n", [binary:encode_hex(KeyPair#keypair.pub)]),
    coin_address:i(KeyPair).

%% ============================================================================

get_file() ->
    case file:read_file(filename()) of
        {ok, Address} ->
            S = string:trim(Address),
            case coin_address:verify(S) of
                true ->
                    coin_address:decode_private(S);
                false ->
                    {error, invalid_stored_key}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

set_file(#keypair{priv = PrivKey} = KeyPair) ->
    case is_valid(KeyPair) of
        true ->
            file:write_file(filename(), [
                coin_address:encode_private(PrivKey),
                $\n
            ]);
        false ->
            {error, invalid_keypair}
    end.

% cache
set_cache(KeyPair) ->
    persistent_term:put(?MODULE, KeyPair).

get_cache() ->
    persistent_term:get(?MODULE, undefined).


-ifdef(TEST).

key_is_valid_test() ->
    ?assert(is_valid(new_keypair())),
    ?assert(is_valid(new_keypair(crypto:strong_rand_bytes(32)))),
    ?assertError(badarg, is_valid(new_keypair(crypto:strong_rand_bytes(31)))),
    ok.

sign_verify_test() ->
    Keypair = new_keypair(),
    Data = crypto:strong_rand_bytes(1024),
    Signature = sign(Data, Keypair#keypair.priv),
    ?assertEqual(true, verify(Data, Signature, Keypair#keypair.pub)),
    ok.


-endif.