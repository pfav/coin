Simple coin
=====

This is a simple blockchain (mostly similar to bitcoin).
I'm writing it mainly to understand this technology.


run in development
==========

    rebar3 as test shell


run multiple nodes
===
    
    # start erlang shell
    $ rebar3 as test shell

    1> PrivateKey = <<0:32/unit:8>>.               % choose a private key
    2> KeyPair = coin_key:new_keypair(PrivateKey). % generate key_pair 
    3> coin_address:i(KeyPair).                    % log formatted private key (sk)
    == address ==
    sk: 5HpHagT65TZzG1PH3CSu63k8DbpvD8s5ip4nEB3kEsreAhHZwdY 51 true
    pk: 124zAJk2sTYLktp6yGZWzuhCu2DGF4H6wK 34 true
    
    # store formattated key as node1 key
    $ echo 5HpHagT65TZzG1PH3CSu63k8DbpvD8s5ip4nEB3kEsreAhHZwdY > node1_key.db
    
    % modify src/chain/coin_block.erl genesis() pubblic key with your own (HEX).
    % otherwise you won't have balance to try and make blocks.
    4> rr(coin_key). % load records
    5> binary:encode_hex(KeyPair#keypair.pub). % show public key HEX and change in genesis()
    <<"3B6A27BCCEB6A42D62A3A8D02A6F0D73653215771DE243A63AC048A18B59DA29">>
    6> q().
    
    # remove old blockchain
    $ rm -rf coin.db
   
    ##### edit node1.toml #####
    cat > node1.toml
    [coin]
    # This node "human" name
    name = "node1"

    [wallet]
    # path on the filesystem to store wallet keys
    path = "node1_key.db"

    [http]
    # admin endpoint
    ip = "127.0.0.1"
    port = 9000
    # number of cowboy acceptors
    acceptor = 4

    [chain.db]
    # path on the filesystem to store database
    path = "node1.db"

    # Add zero or more to connect and monitor
    [[peer]]
    ip = "127.0.0.2"
    port = 9000

    ##### edit node2.toml ######
    cat > node2.toml
    [coin]
    # This node "human" name
    name = "node2"

    [wallet]
    # path on the filesystem to store wallet keys
    path = "node2_key.db"

    [http]
    # admin endpoint
    ip = "127.0.0.2"
    port = 9000
    # number of cowboy acceptors
    acceptor = 4

    [chain.db]
    # path on the filesystem to store database
    path = "node2.db"

    # Add zero or more to connect and monitor
    [[peer]]
    ip = "127.0.0.1"
    port = 9000
    
    # for starting node1 run
    $ COIN_CONFIG_PATH=node1.toml rebar3 as test shell

    # for starting node2 run
    $ COIN_CONFIG_PATH=node2.toml rebar3 as test shell

    # open http://127.0.0.1:9000 and http://127.0.0.2:9000 and make some blocks!


configuration
---

See `priv/default.toml`
