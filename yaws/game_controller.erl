-module(game_controller).
-define(GAME_SERVER, ?MODULE).
-behaviour(gen_server).

% Application API
-export([start/0, new_game/1, add_player/1, waiting_list/0, stop/0]).

% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start() ->
	gen_server:start_link({local, ?GAME_SERVER}, ?MODULE, [], []).

new_game(User) ->
	gen_server:call(?GAME_SERVER, {new_game, User}).

add_player(User) ->
	gen_server:call(?GAME_SERVER, {add_player, User}).

waiting_list() ->
	gen_server:call(?GAME_SERVER, waiting_list).

stop() ->
	gen_server:call(?GAME_SERVER, stop).

init([]) ->
	{ok, ets:new(game_sessions, [set])}.

handle_call({new_game, User}, _From, State) ->
	{ok, Game} = game:start(User),
	ets:insert(State, {Game, waiting}),
	{reply, {ok, {Game, waiting}}, State};

handle_call({add_player, Game, User}, _From, State) ->
	{ok, _} = game:add_player(Game, User),
	ets:insert(State, {Game, playing}),
	{reply, {ok, {Game, playing}}, State};

handle_call(waiting_list, _From, State) ->
	W = [ Game || {Game, _S} <- ets:match_object(State, {'_', waiting})],
	{reply, {ok, W}, State}.

handle_cast(stop, State) ->
	ets:delete(State),
	{stop, normal, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_R, _S) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.
