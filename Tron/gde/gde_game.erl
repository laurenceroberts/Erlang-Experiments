-module(gde_game).
-export([game_loop/2]).

-record(match, {key, max_players, status, rounds, players}).
-record(player, {pid, name=none, hash, mode}).

game_loop(Matches, Players) ->
	receive
		{player_connect, Pid, Name} ->
			Player = #player{pid=Pid, name=Name, hash=Name, mode=connect},
			NewPlayers = [{Player#player.hash, Player} | Players],
			NewMatches = Matches,
			% gde_router:handle_websocket(send, "jelly babies"),
			io:format("gde_game:player_connect -- ~p~n", [Name]);
		
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
		
		{match_create, MaxPlayers} ->
			Match = #match{max_players=MaxPlayers, status=lobby, rounds=1},
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
		
	end,
	io:format("gde_game:received~n"),
	game_loop(NewMatches, NewPlayers).

find_match(Matches) ->
	% Loop matches, check for empty spaces, drop into first found with less than 4 players
	lists:foreach(fun(Match) ->
		if
			erlang:length(Match#match.players) == 4 ->
				skip;
			true -> 
				Match
		end
	end, Matches).