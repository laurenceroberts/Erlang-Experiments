-module(gde_game).
-export([game_loop/2]).

-record(match, {key, max_players, status, round, players}).
-record(player, {ws_pid, name=none, hash, mode}).

game_loop(Matches, Players) ->
	receive
		%% PLAYER JOIN
		{player_connect, WsPid, Name} ->
			Player = #player{ws_pid=WsPid, name=Name, hash=Name, mode=connect},
			NewPlayers = [{Player#player.hash, Player} | Players],
			
			if
				(length(Matches) =:= 0) ->
					Match = #match{players=[{Player#player.hash, Player}], max_players=4, status=lobby, round=0},
					NewMatches = [{Match#match.key, Match} | Matches];
				true ->
					Match = gde_game:find_match(Matches),
					UMatch = Match#match{players=[Player | Match#match.players]},
					NewMatches = [{UMatch#match.key, UMatch} | proplists:delete(UMatch#match.key, Matches)],
					NewMatches = Matches
			end,
			Player#player.ws_pid ! {send, "player_connected:" ++ Player#player.hash},
			gde_game:send_matches(NewMatches, NewPlayers),
			io:format("gde_game:player_connect -- ~p ~p ~n", [WsPid, Name]);
		
		{player_join_match, PlayerHash, MatchKey} ->
			Player = proplists:get_value(PlayerHash, Players),
			Match = proplists:get_value(MatchKey, Matches),
			UMatch = Match#match{players=[Player | Match#match.players]},
			NewPlayers = Players,
			NewMatches = [{UMatch#match.key, UMatch} | proplists:delete(MatchKey, Matches)],
			io:format("gde_game:player_join_match -- ~p~n", [Player#player.name]);
		
		{player_leave_match, PlayerHash, MatchKey} ->
			Match = proplists:get_value(MatchKey, Matches),
			UMatch = Match#match{players=proplists:delete(PlayerHash, Players)},
			NewPlayers = Players,
			NewMatches = [{UMatch#match.key, UMatch} | proplists:delete(MatchKey, Matches)];
			% io:format("gde_game:player_leave_match -- ~p~n", [proplists:get_value(PlayerHash, Players)#player.Name]);
		
		
		%% PLAYER CONTROL
		{player_control_dir, TargetDir} ->
			NewPlayers = Players,
			NewMatches = Matches,
			io:format("gde_game:player_control_dir -- ~p~n", [TargetDir]);
		
		
		%% MATCH
		{match_create, MaxPlayers} ->
			Match = #match{max_players=MaxPlayers, status=lobby, round=0},
			NewPlayers = Players,
			NewMatches = [{Match#match.key, Match} | Matches],
			io:format("gde_game:match_create -- ~p~n", [MaxPlayers]);
		
		{match_start, Key} ->
			NewPlayers = Players,
			NewMatches = Matches,
			io:format("gde_game:match_start -- ~p~n", [Key]);
			
		{match_round_start, Key} ->
			NewPlayers = Players,
			NewMatches = Matches,
			io:format("gde_game:match_start -- ~p~n", [Key]);
			
		{match_round_finish, Key} ->
			NewPlayers = Players,
			NewMatches = Matches,
			io:format("gde_game:match_start -- ~p~n", [Key]);
			
		{match_finish, Key} ->
			NewPlayers = Players,
			NewMatches = Matches,
			io:format("gde_game:match_start -- ~p~n", [Key]);
			
		{match_delete, Key} ->
			NewPlayers = Players,
			NewMatches = Matches,
			io:format("gde_game:match_start -- ~p~n", [Key])
	
	% after 2000 ->
	% 		NewPlayers = Players,
	% 		NewMatches = Matches,
	% 		send_matches(Matches, Players)
	end,
	% io:format("gde_game:received~n"),
	gde_game:game_loop(NewMatches, NewPlayers).

% Loop matches, check for empty spaces, drop into first found with less than 4 players
find_match(Matches) ->
	catch lists:foreach(fun(Match) ->
		io:format("~p~n", [Match]),
		if
			length(Match#match.players) < 4 ->
				throw(Match);
			true ->
				skip
		end
	end, Matches).
	

% Build matchlist and send to every player
send_matches(Matches, Players) ->
	MatchList = lists:map(fun({_, Match}) -> 
		gde:string_format("max_players=~p;num_players=~p;status=~p;round=~p;player_list=~p", [Match#match.max_players, erlang:length(Match#match.players), Match#match.status, Match#match.round,
			lists:map(fun({_, Player}) ->
				gde:string_format("~p,~p", [Player#player.name, Player#player.mode])
			end, Match#match.players)
		])
	end, Matches),
	
	lists:foreach(fun({_, Player}) ->
		Player#player.ws_pid ! {send, "matchlist::" ++ string:join(MatchList, "|")}
	end, Players).