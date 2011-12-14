%%% Debug
-module(gde_debug).
-export([start/1, stop/0]).

% start http server
start(Port) ->
	misultin:start_link([{port, Port}, {loop, fun(Req) -> handle_http(Req) end}]).

% stop server
stop() ->
	misultin:stop().

% callback on request received
handle_http(Req) ->
	% get params depending on method
	Method = Req:get(method),
	Args = case Method of
		'GET' ->
			Req:parse_qs();
		'POST' ->
			Req:parse_post()
	end,
	% build an XML with all parameters and values
	BuildXml = fun({Param, Value}, Acc) ->
		[lists:flatten(io_lib:format("<param><name>~s</name><value>~s</value></param>", [Param, Value]))|Acc]
	end,
	Xml = lists:flatten(lists:reverse(lists:foldl(BuildXml, [], Args))),
	% output
	Req:ok([{"Content-Type", "text/xml"}], "<misultin_test><method>~s</method>~s</misultin_test>", [Method, Xml]).
