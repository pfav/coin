-ifndef(__COIN_HRL__).
-define(__COIN_HRL__, true).

-include_lib("kernel/include/logger.hrl").

-record(keypair, {
    priv :: coin_key:private_key() | undefined,
    pub  :: coin_key:public_key()
}).

-record(branch, {
    latest_hash           :: coin_block:hash(),
    current_difficulty    :: non_neg_integer(),
    cumulative_difficulty :: non_neg_integer(),
    length                :: non_neg_integer(),
    coinbase              :: non_neg_integer(),
    supply                :: non_neg_integer()
}).

-record(block, {
    version    :: non_neg_integer(),
    index      :: non_neg_integer(),
    hash       :: coin_block:hash() | undefined,
    prev_hash  :: coin_block:hash(),
    timestamp  :: non_neg_integer(),
    txs        :: [coin_tx:tx()] | undefined,
    nonce      :: non_neg_integer(),
    difficulty :: non_neg_integer()
}).

-record(tx, {
    hash     :: coin_tx:hash() | undefined,
    in  = [] :: [ coin_tx:in() ],
    out = [] :: [ coin_tx:out() ]
}).

-record(tx_in, {
    out_hash  :: coin_tx:hash() | undefined,
    out_index :: non_neg_integer(),
    signature :: binary() | undefined
}).

-record(tx_out, {
    address :: coin_key:public_key(),
    amount  :: non_neg_integer()
}).

-define(BRANCH_PREFIX, <<$c>>).
-define(BLOCK_HASH_PREFIX, <<$c>>).
-define(BLOCK_INDEX_KEY_PREFIX, <<$i>>).
-define(TX_PREFIX, <<$t>>).

% coin_db keys
-define(BRANCH_KEY(N),           <<$c, (N)>>).
-define(BLOCK_HASH_KEY(Hash),    <<$b, (Hash)/binary>>).
-define(BLOCK_INDEX_KEY(Idx),    <<$i, (Idx):64>>).
-define(TX_KEY( Hash),           <<$t, (Hash)/binary>>).

-define(BLOCK_GENERATION_INTERVAL, 10).
-define(DIFFICULTY_ADJUSTMENT_INTERVAL, 10).
-define(COINBASE_GENERATION, 2022).
-define(MAX_TX_PER_BLOCK, 1024).
-ifndef(CID).
-define(CID, 10_000_000_000).
-endif.
-define(COINBASE_AMOUNT, 50 * ?CID).

-define(DEFAULT_PEER_PORT, 34022).
-define(DEFAULT_PEER_IP, any).

-endif.
