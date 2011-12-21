-module(gde).
-export([start/1, stop/0, string_format/2]).

start(Port) ->
	gde_router:start(Port).

stop() ->
	gde_router:stop().


string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).