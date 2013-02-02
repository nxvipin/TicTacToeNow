-module(game_controller).
-define(GAME_SERVER, ?MODULE).
-behaviour(gen_server).

% Application API
-export([start/0, new_game/1, add_player/1, stop/1]).

% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start() ->
	gen_server:start_link({local, ?GAME_SERVER}, ?MODULE, [], []).

new_game(User) ->
	gen_server:call(?GAME_SERVER, {new_game, User}).

add_player(User) ->
	gen_server:call(?GAME_SERVER, {add_player, User}).

stop() ->
	gen_server:call(?GAME_SERVER, stop).

init([]) ->
	{ok, ets:new(game_sessions, [set])}.

handle_call({new_game, User}, _From, State) ->
	{ok, Game} = game:new(User),
	{reply, {ok, {Game, waiting}}, ets:insert(State, {Game, waiting})};

handle_call({add_player, Game, User}, _From, State) ->
	{ok, _} = game:add_player(Game, User),
	{reply, {ok, {Game, playing}, ets:insert(State, {Game, playing})};

handle_cast(stop, State) ->
	ets:delete(State),
	{stop, normal, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_R, _S) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.
