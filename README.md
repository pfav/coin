Simple coin
=====

This is a simple blockchain (mostly similar to bitcoin).
I'm writing it mainly to understand this technology.


run in development
==========

    rebar3 as test shell


run multiple nodes
===

    $ cp priv/default.toml node{1,2}.toml
    # edit node1 and node2
    
    # on node1 run
    $ COIN_CONFIG_PATH=node1.toml rebar3 as test shell

    # on node2 run
    $ COIN_CONFIG_PATH=node1.toml rebar3 as test shell


configuration
---

See `priv/default.toml`
