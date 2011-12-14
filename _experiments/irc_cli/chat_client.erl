-module(chat_client).
%-export([connect/5, handler/5, disconnected/2, start_connector/3, try_to_connect/4, wait_login_response/1, active/1]).
-compile(export_all).

connect(Host, Port, HostPass, Group, Nick) ->
	spawn(fun() -> handler(Host, Port, HostPass, Group, Nick) end).

handler(Host, Port, HostPass, Group, Nick) ->
	start_connector(Host, Port, HostPass),
	disconnected(Group, Nick).

% actions to take when disconnected
disconnected(Group, Nick) ->
	receive
		{connected} ->
			% set message on client to confirm connection
			io:format("Connected to server~nSending data...~n"),
			% not sure where this is
			lib_chan_mm:send(MM, {login, Group, Nick}),
			% wait for login
			wait_login_response(MM);
		{destroyed} ->
			exit(died);
		{status, S} ->
			% update status
			disconnected(Group, Nick);
		Other ->
			io:format("Chat client disconnected unexpected:~p~n", [Other]),
			disconnected(Group, Nick)
	end.

start_connector(Host, Port, Pass) ->
	S = self(),
	spawn_link(fun() -> try_to_connect(S, Host, Port, Pass) end).

try_to_connect(Parent, Host, Port, Pass) ->
	%% Parent is the Pid of the process that spawned this process
	case lib_chan:connect(Host, Port, chat, Pass, []) of
		{error, _Why} ->
			Parent ! {status, {cannot, connect, Host, Port}},
			% Sleep for 2 seconds then retry connection
			sleep(2000),
			try_to_connect(Parent, Host, Port, Pass);
		{ok, MM} ->
			lib_chan_mm:controller(MM, Parent),
			Parent ! {connect, MM},
			exit(connectorFinished)
	end.

wait_login_response(MM) ->
	receive
		{chan, MM, ack} ->
			active(MM);
		Other ->
			io:format("Chat client login unexpected:~p~n", [Other]),
			wait_login_response(MM)
	end.

active(MM) ->
	receive
		{Nick, Str} ->
			lib_chan_mm:send(MM, {relay, Nick, Str}),
			active(MM);
		{chan, MM, {msg, From, Pid, Str}} ->
			io:format("~p > ~p~n", [Nick, Str]),
			active(MM);
		{close, MM} ->
			exit(serverDied);
		Other ->
			io:format("Chat client active unexpected:~p~n", [Other]),
			active(MM)
	end.
