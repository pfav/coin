-module(coin_env).
-author("pfav").

%% API
-export([
    init/0,
    get_config/1,
    get_config/0
]).

%% DEBUG
-export([
    set_config/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).
-endif.

% Config for NOW is READ_ONLY

-include_lib("kernel/include/logger.hrl").

% init/0 ----------------------------------------------------------------------

-spec init() ->
    ok | no_return().

init() ->
    load_config().

% get_config/0 ----------------------------------------------------------------

-spec get_config() ->
    #{}.

get_config() ->
    persistent_term:get(?MODULE).

% get_config/1 ----------------------------------------------------------------

-spec get_config(term()) ->
    {ok, term()} | {error, not_found}.

get_config(Key) ->
    tomerl:get(get_config(), Key).

%% private --------------------------------------------------------------------

set_config(Config) when is_map(Config) ->
    persistent_term:put(?MODULE, Config).

load_config() ->
    Path = config_path(),
    DefaultPath = default_config_path(),

    ?LOG_NOTICE("[ENV] loading configuration ~s", [Path]),
    Config = load_config_result(Path, DefaultPath, tomerl:read_file(Path)),
    set_config(Config).

load_config_result(_, _, {ok, Config}) ->
    Config;
load_config_result(Path, Path, {error, enoent}) ->
    ?LOG_NOTICE("[ENV] loading default configuration"),
    load_config_result(Path, Path, tomerl:read_file(Path));
load_config_result(Path, DefaultPath, {error, enoent}) ->
    ?LOG_WARNING("[ENV] missing ~p. creating...", [Path]),
    {ok, Bin} = file:read_file(DefaultPath),
    ok = file:write_file(Path, Bin),
    load_config_result(Path, DefaultPath, tomerl:read_file(Path));
load_config_result(Path, DefaultPath, {error, Reason}) ->
    ?LOG_CRITICAL("[ENV] failed to load ~s reason ~p", [Path, Reason]),
    error(Reason, [Path, DefaultPath, {error, Reason}]).

config_path() ->
    os:getenv("COIN_CONFIG_PATH", default_config_path()).

default_config_path() ->
    filename:join([coin_util:priv(), "default.toml" ]).