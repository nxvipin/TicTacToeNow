-module(game_event_manager).
-define(GEM, ?MODULE).

-export([start/0, add_handler/1, waiting_list/0]).

start() ->
	gen_event:start_link({local, ?GEM}).

add_handler({Mod, Options}) ->
	gen_event:add_handler(?GEM, Mod, Options).

waiting_list() ->
	gen_event:call(?MODULE, game_event_handler, waiting_list).

stop() ->
	gen_event:stop(?GEM).

