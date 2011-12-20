-module(gde_router).
-export([start/1, stop/0, handle_http/2, handle_websocket/1]).

start(Port) ->
	register(game_loop, spawn(fun() -> 
		gde_game:game_loop([], [])
	end)),
	misultin:start_link([
		{port, Port},
		{loop, fun(Req) -> gde_router:handle_http(Req, Port) end},
		{ws_loop, fun(Ws) -> gde_router:handle_websocket(Ws) end},
		{ws_autoexit, true}
	]).

stop() ->
	unregister(game_loop),
	misultin:stop().

% call handle with request method and list of url parts
handle_http(Req, _Port) ->
	route(Req:get(method), Req:resource([lowercase, urldecode]), Req).

%% Web Sockets
handle_websocket(Ws) ->
	receive
		{browser, "player_connect:" ++ Name} ->
			game_loop ! {player_connect, self(), Name};
		
		{browser, "player_join_match:" ++ PlayerHash} ->
			game_loop ! {player_join_match, PlayerHash};
		
		{send, Data} ->
			% io:format("gde_router:handle_websocket -- sending data ~p~n", [Data]),
			Ws:send(Data)
	end,
	gde_router:handle_websocket(Ws).


%% HOOKS

% index
route('GET', [], Req) ->
	Req:file(gde_config:site_path() ++ "index.htm");

% 404
route(_, Url, Req) ->
	io:format("Error 404: ~p~n", [Url]),
	Req:file(gde_config:site_path() ++ "404.htm").