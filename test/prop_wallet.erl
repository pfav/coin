-module(prop_wallet).
-author("pfav").

-include_lib("proper/include/proper.hrl").
-compile([export_all, nowarn_export_all]).

-include_lib("coin/include/coin.hrl").

range_binary(B, E) ->
    ?LET(N, range(B, E), crypto:strong_rand_bytes(N)).


%% coin_base58 ----------------------------------------------------------------

prop_base58(doc) ->
    "encoding/decoding base58 failed".

prop_base58() ->
    ?FORALL(Bin, range_binary(0, 1024),
        begin
            Bin == coin_base58:decode(coin_base58:encode(Bin))
        end).

%% coin_address -------------------------------------------------------------

prop_public_address_verification(doc) ->
    "public address verification failed".

prop_public_address_verification() ->
    ?FORALL(K, range_binary(0, 1024),
    begin
        coin_address:verify(coin_address:encode_public(K))
    end).


prop_private_address_verification(doc) ->
    "private address verification failed".

prop_private_address_verification() ->
    ?FORALL(K, range_binary(0, 1024),
        begin
            A = coin_address:encode_private(K),
            coin_address:verify(A)
        end).

prop_private_address_decoding(doc) ->
    "private address decoding failed".

prop_private_address_decoding() ->
    ?FORALL(K, range_binary(0, 1024),
        begin
            A = coin_address:encode_private(K),
            {ok, K} == coin_address:decode_private(A)
        end).

%% coin_key

prop_sign_verify(doc) ->
    "failed to sign/verify".

keypair() ->
    ?LET(Bin, binary(32), coin_key:new_keypair(Bin)).

prop_sign_verify() ->
    ?FORALL(KeyPair, keypair(),
        ?FORALL(Bin, range_binary(0, 1024),
            begin
                Signature = coin_key:sign(Bin, KeyPair),
                coin_key:verify(Bin, Signature, KeyPair#keypair.pub)
            end)).

%%prop_base58(opts) ->
%%%    %% Override CLI and rebar.config option for `numtests' only
%%    [{numtests, 5000}].

%?assert( proper:quickcheck(Prop, [{to_file, user}, {numtests, 500}]) ),