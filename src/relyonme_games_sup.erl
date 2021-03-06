-module(relyonme_games_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	childspec/1,
	list_current_games/0
	]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, { {one_for_all, 0, 1}, []} }.

list_current_games() ->
	[{GameNumber, relyonme_game_room:get_room_joinable(Pid)} || {GameNumber, Pid,_,_} <- supervisor:which_children(?MODULE)].


childspec(PlayerConnection) ->
	RoomNumber = proplists:get_value(workers, supervisor:count_children(?MODULE)),
	Args = [
		RoomNumber,
		PlayerConnection
	],
	{RoomNumber,
        {relyonme_game_room, start_link, Args},
        permanent, 5000, worker, [relyonme_game_room]}.
