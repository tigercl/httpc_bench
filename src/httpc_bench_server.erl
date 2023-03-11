-module(httpc_bench_server).
-include("httpc_bench.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    start/0,
    stop/0
]).

%% public
start() ->
    case get(?MODULE) of
        undefined ->
            Sleep = sleep(),
            io:format("HTTP Server started on port ~p, sleep=~p...~n", [?PORT, Sleep]),
            {ok, LSocket} = listen(),
            put(?MODULE, LSocket),
            spawn(fun () -> accept(LSocket, Sleep) end),
            ok;
        _LSocket ->
            {error, already_started}
    end.

stop() ->
    case get(?MODULE) of
        undefined ->
            {error, not_started};
        LSocket ->
            ok = gen_tcp:close(LSocket),
            put(?MODULE, undefined),
            ok
    end.

%% private
accept(LSocket, Sleep) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            spawn_link(
                fun() ->
                    io:format(user, "started one server process ~p, sleep=~p\n", [Socket, Sleep]),
                    loop(Socket, Sleep, <<>>)
                end),
            accept(LSocket, Sleep);
        {error, _Reason} ->
            ok
    end.

sleep() ->
   case os:getenv("SLEEP_PER_REQ") of
       false -> 10;
       "" -> 10;
       N -> list_to_integer(N)
   end.

count_requests([<<>>], N) ->
    {N, <<>>};
count_requests([Buffer], N) ->
    {N, Buffer};
count_requests([_ | T], N) ->
    count_requests(T, N + 1).

listen() ->
    Self = self(),
    spawn(fun () ->
        Options = [binary, {backlog, 4096}, {active, false}, {reuseaddr, true}],
        Self ! gen_tcp:listen(?PORT, Options),
        receive
            _ ->
                ok
        end
    end),
    receive
        {ok, LSocket} ->
            {ok, LSocket};
        Other ->
            io:format(user, "unexpected listen result: ~p\n", [Other]),
            exit(kill)
    end.

loop(Socket, Sleep, Buffer) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Split = binary:split(<<Buffer/binary, Data/binary>>,
                <<"\r\n\r\n">>, [global]),
            {N, Buffer2} = count_requests(Split, 0),
            Responses = [
                <<"HTTP/1.1 200 OK\r\n",
                  "Server: httpc_bench\r\n",
                  "Date: Tue, 07 Mar 2017 01:10:09 GMT\r\n",
                  "Content-Length: 12\r\n\r\n",
                  "httpc_bench!">> || _ <- lists:seq(1, N)],
            lists:foreach(
                fun(R) ->
                        case gen_tcp:send(Socket, R) of
                            ok -> ok;
                            {error, _} -> exit(normal)
                        end,
                        timer:sleep(Sleep)
                end, Responses),
            loop(Socket, Sleep, Buffer2);
        {error, Reason} ->
            io:format(user, "stopped one server process ~p: ~0p\n", [Socket, Reason]),
            exit(normal)
    end.
