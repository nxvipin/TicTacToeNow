-module(game_event_handler).
-define(GEH, ?MODULE).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,terminate/2]).


init([]) ->
	{ok, ets:new(game_sessions, [set, public])}.

handle_event({new_game, PID}, State) ->
	ets:insert(State, {PID, waiting}),
	{ok, State};

handle_event({add_player, PID}, State) ->
	ets:insert(State, {PID, playing}),
	{ok, State};

handle_event(Event, State) ->
	io:format("Event ~p~n",[Event]),
	{ok, State}.

handle_call(waiting_list, State) ->
	W = [ Game || {Game, _S} <- ets:match_object(State, {'_', waiting})],
	{ok, W, State}.

handle_info(_, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
