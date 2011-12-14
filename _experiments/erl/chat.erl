-module(chat).
-export([listen/1]).

%% TCP options for our listening socket. The initial list atom
%% specifies that we should receive data as lists of bytes (ie
%% strings) rather than binary objects.

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(user, {name=none, socket, mode, message=[]}).

%% Listen on the given port, accept the first incoming connection and
%% launch the echo loop on it. This also needs to launch the
%% client_manager process, since it's our server's entry point.

listen(Port) ->
	Pid = spawn(fun() -> manage_clients([]) end),
	register(client_manager, Pid),
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	do_accept(LSocket).


%% Accepting a connection gives us a connection socket with the
%% newley-connected client on the other end. Since we want to accept
%% more than one client, we spawn a new process for each and then wait
%% for another connection on our listening socket.

do_accept(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			spawn(fun() -> handle_client(Socket) end),
			client_manager ! {connect, Socket};
		{error, Reason} ->
			io:fwrite("Socket accept error: ~s~n", [Reason])
	end,
	do_accept(LSocket).


%% All the client-socket process needs to do is wait for data and
%% forward it to the client_manager process which decides what to do
%% with it. If the client disconnects, we let client_manager know and
%% then quietly go away.

handle_client(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			client_manager ! {data, Socket, Data},
			handle_client(Socket);
		{error, closed} ->
			client_manager ! {disconnect, Socket}
	end.


%% This is the main loop of the client_manager process. It maintains
%% the list of users and calls the handler for client input.

manage_clients(Users) ->
	io:fwrite("Users:~n"),
	lists:foreach(fun(U) -> io:fwrite(">>> ~w~n", [U]) end, Users),
	receive
		{connect, Socket} ->
			User = #user{socket=Socket, mode=connect},
			send_prompt(User),
			io:fwrite("User connected: ~w~n", [User]),
			NewUsers = [User | Users];
		{disconnect, Socket} ->
			User = find_user(Socket, Users),
			io:fwrite("User disconnected: ~w~n", [User]),
			NewUsers = lists:delete(User, Users);
		{data, Socket, Data} ->
			User = find_user(Socket, Users),
			NewUsers = parse_data(User, Users, Data),
			NewUser = find_user(Socket, NewUsers),
			send_prompt(NewUser)
	end,
	manage_clients(NewUsers).


%% find_user is a utility function to get a user record associated
%% with a particular socket out of the user list.

find_user(Socket, Users) ->
	{value, User} = lists:keysearch(Socket, #user.socket, Users),
	User.


%% delete_user returns the user list without the given user. It
%% deletes the user from the list based on the socket rather than
%% the whole record because the list might hold a different version.

delete_user(User, Users) ->
	lists:keydelete(User#user.socket, #user.socket, Users).


%% Sends an appropriate prompt to the user. e.g. "Name: " when the
%% user connects.

send_prompt(User) ->
	case User#user.mode of
		connect ->
			gen_tcp:send(User#user.socket, "Enter your name: ");
		active ->
			ok
	end.


%% Send the given data to all the users in the active mode.

send_to_active(Prefix, Users, Message) ->
	ActiveUsers = lists:filter(fun(U) -> U#user.mode == active end, Users),
	lists:foreach(fun(U) -> gen_tcp:send(U#user.socket, Prefix ++ Message) end, ActiveUsers),
	ok.


%% Parse data

parse_data(User, Users, Data) ->
	case User#user.mode of
		active ->
			send_to_active("["++get_time()++"]" ++ User#user.name ++ ": ", delete_user(User, Users), Data),
			Users;
		connect ->
			send_to_active("--chat ["++get_time()++"]: ", Users, clean_text(Data) ++ " connected\n"),
			UUser = User#user{name=clean_text(Data), mode=active},
			[UUser | delete_user(User, Users)]
	end.


%% Get Time

get_time() ->
	{H, M, S} = time(),
	io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]).

%% Clean text. Returns the first continuous block of non-matching characters
%% rather than stripping matching characters off the front and back.

clean_text(String) ->
	clean_text(String, "\r\n\t ").

clean_text(String, Chars) ->
	[Stripped|_Rest] = string:tokens(String, Chars),
	Stripped.