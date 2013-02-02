-module(game).
-behaviour(gen_server).

% Application API
-export([start/1, add_player/2, move/2, get_state/1]).

% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% play is true if the game can continue and false if the game ends.
% player1 always plays 'X'
% player2 always plays 'O'
% next_player is either 'X' or 'O'
-record(game, {	play = true,
				player1 = none,
				player2 = none,
				next_player = "X",
				board=["*", "*", "*", "*", "*", "*", "*", "*", "*"]}).

% API Definition
start({Player, true}) ->
	gen_server:start_link(?MODULE, [{Player, true}], []);
start({Player, false}) ->
	gen_server:start_link(?MODULE, [{Player, false}], []);
start(Player) ->
	start({Player, true}).

add_player(Server, Player) ->
	gen_server:call(Server, {add_player, Player}).


move(Server, {Player, Move}) ->
	gen_server:call(Server, {move, Player, Move}).

get_state(Server) ->
	gen_server:call(Server, get_state).


% Gen Server Callbacks
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
	{reply, {ok, S}, S};
handle_call({move, Player, Move}, _From, State) ->
	Board = State#game.board,
	Play = State#game.play,
	Next_Player_Symbol = State#game.next_player,
	Is_Valid_Move = board_check_valid_move(Board, Move),
	Next_Player_Id = Player,
	PlayerID = board_get_id_from_symbol(State, Next_Player_Symbol),
	if 
		Is_Valid_Move and (PlayerID == Next_Player_Id) and Play ->
			B = board_move(Board, Next_Player_Symbol, Move),
			N = other(Next_Player_Symbol),
			Full = board_check_full(B),
			WinX = board_check_win(B, "X"),
			WinO = board_check_win(B, "O"),
			if
				Full or WinX or WinO ->
					PlayNext = false;
				true ->
					PlayNext = true
			end,
			S = State#game{play= PlayNext, next_player=N, board=B},
			{reply, {ok, S}, S};
		true ->
			{reply, {error, [Is_Valid_Move, PlayerID, Next_Player_Id, Play]}, State}
	end;
handle_call(get_state, _From, State) ->
	{reply, {ok, State}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_R, _S) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.


% Helper Functions
board_check_valid_move(Board,Move) ->
	lists:nth(Move, Board) == "*".

board_valid_moves(Board) ->
	{Valid_Moves,_} = lists:partition(fun(X) -> board_check_valid_move(Board,X) end,lists:seq(1,9)),
	Valid_Moves.

board_check_win(Board, S) ->
	case Board of
		[S,_,_,S,_,_,S,_,_] -> true;
		[_,S,_,_,S,_,_,S,_] -> true;
		[_,_,S,_,_,S,_,_,S] -> true;
		[S,S,S,_,_,_,_,_,_] -> true;
		[_,_,_,S,S,S,_,_,_] -> true;
		[_,_,_,_,_,_,S,S,S] -> true;
		[S,_,_,_,S,_,_,_,S] -> true;
		[_,_,S,_,S,_,S,_,_] -> true;
		_ -> false
	end.

board_check_full(Board) ->
	not lists:any(fun(X) -> X == "*" end, Board).

other(Player) ->
	if	Player == "X" -> "O";
		Player == "O" -> "X"
	end.

board_move(Board, Player, Move) ->
	{Head, Tail} = lists:split(Move-1,Board),
	lists:append([Head,[Player],lists:nthtail(1,Tail)]).

board_get_id_from_symbol(State, Symbol) ->
	if
		Symbol == "X" ->
			State#game.player1;
		true ->
			State#game.player2
	end.
