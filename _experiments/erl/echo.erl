-module(echo).
-export([listen/1]).

%% TCP options for our listening socket. The initial list atom
%% specifies that we should receive data as lists of bytes (ie
%% strings) rather than binary objects.

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).


%% Listen on the given port, accept the first incoming connection and
%% launch the echo loop on it.

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    {ok, Socket} = gen_tcp:accept(LSocket),
    do_echo(Socket).


%% Sit in a loop, echoing everything that comes in on the socket.
%% Exits cleanly on client disconnect.

do_echo(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            do_echo(Socket);
        {error, closed} ->
            ok
    end.