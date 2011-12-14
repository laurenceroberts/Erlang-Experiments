-module(basicchat).
-export([listen/1]).

%% TCP options for our listening socket. The initial list atom
%% specifies that we should receive data as lists of bytes (ie
%% strings) rather than binary objects.

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).


%% Listen on the given port, accept the first incoming connection and
%% launch the echo loop on it. This also needs to launch the
%% client_manager process, since it's our server's entry point.

listen(Port) ->
    Pid = spawn(fun() -> manage_clients([]) end),
    register(client_manager, Pid),
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    do_accept(LSocket).


%% The accept gets its own function so we can loop easily.
%% Tail recusion. We also need to let client_manager know
%% that there's a new socket to manage.

do_accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> handle_client(Socket) end),
    client_manager ! {connect, Socket},
    do_accept(LSocket).


%% Disconnects notify client manager that the socket is no longer
%% open and data is sent as-is to be distributed.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            client_manager ! {data, Data},
            handle_client(Socket);
        {error, closed} ->
            client_manager ! {disconnect, Socket}
    end.


%% Mantain a list of sockets, handle connect and disconnect messages
%% and distribute data between them.

manage_clients(Sockets) ->
    receive
        {connect, Socket} ->
            io:fwrite("Socket connected: ~w~n", [Socket]),
            NewSockets = [Socket | Sockets];
        {disconnect, Socket} ->
            io:fwrite("Socket disconnected: ~w~n", [Socket]),
            NewSockets = lists:delete(Socket, Sockets);
        {data, Data} ->
            send_data(Sockets, Data),
            NewSockets = Sockets
    end,
    manage_clients(NewSockets).


%% Send data to all sockets in the list. This is done by constructing
%% a closure around gen_tcp:send and the data and then passing that to
%% lists:foreach/2 with the list of sockets.

send_data(Sockets, Data) ->
    SendData = fun(Socket) ->
                        gen_tcp:send(Socket, Data)
                end,
    lists:foreach(SendData, Sockets).