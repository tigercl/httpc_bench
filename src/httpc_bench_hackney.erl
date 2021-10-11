% https://github.com/benoitc/hackney

-module(httpc_bench_hackney).
-include("httpc_bench.hrl").

-export([
    get/0,
    start/1,
    stop/0
]).

%% public
get() ->
    Options = [{pool, httpc_bench}],
    case hackney:request(get, ?URL, ?HEADERS, <<>>, Options) of
        {ok, _, _, Ref} ->
            case hackney:body(Ref) of
                {ok, _} -> ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

start(PoolSize) ->
    application:ensure_all_started(hackney),
    HackneyOps = [
        {pool_size, PoolSize},
        {timeout, ?TIMEOUT}
    ],
    ok = hackney_pool:start_pool(httpc_bench, HackneyOps).

stop() ->
    ok = hackney_pool:stop_pool(httpc_bench).
