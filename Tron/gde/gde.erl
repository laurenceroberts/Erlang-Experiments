-module(gde).
-export([start/1, stop/0]).

start(Port) ->
	gde_router:start(Port).

stop() ->
	gde_router:stop().