-module(game).
-behaviour(gen_server).

% Application API
-export([start/1, add_player/2]).

% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Player1 always plays 'X'
% Player2 always plays 'O'
% Next_Player is either 'X' or 'O'
-record(game, {	player1 = none,
				player2 = none,
				next_player = "X",
				board=["*", "*", "*", "*", "*", "*", "*", "*", "*"]}).

start(Player) ->
	start({Player, true});
start({Player, true}) ->
	gen_server:start_link(?MODULE, [{Player, true}], []);
start({Player, false}) ->
	gen_server:start_link(?MODULE, [{Player, false}], []);

add_player(PID, Player) ->
	gen_server:call(PID, {add_player, Player}).


init([{Player, Start}]) ->
	State = case Start of
				true -> #game{player1 = Player};
				_ -> #game{player2 = Player}
			end,
	{ok, State}.

handle_call({add_player, Player}, _From, State) ->
	Player1 = State#game.player1,
	Player2 = State#game.player2,
	S = if
			Player1 == none -> State#game{player1 = Player};
			Player2 == none -> State#game{player2 = Player};
			true -> State
		end,
	{reply, {ok, State}, S};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_R, _S) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.

