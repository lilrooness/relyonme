-module(relyonme_enemy_sup).

-behaviour(supervisor).

-export([
	start_link/0,
	init/1,
	new_enemy/2,
	update_get_enemy_positions/1]).

-export([new_enemy/2, update_get_enemy_positions/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => relyonme_enemy,
                    restart => temporary,
                    start => {relyonme_enemy, start_link, []},
                    shutdown => brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

new_enemy(Pid, StartingPos) ->
    {ok, ChildId} = supervisor:start_child(Pid, [StartingPos]),
    relyonme_enemy:set_velocity(ChildId, {rand:uniform()*2-1, rand:uniform()*2-1}).

update_get_enemy_positions(Pid) ->
	Children = supervisor:which_children(Pid),
    lists:map(fun(EnemyPid) ->
        {_X, _Y} = gen_server:call(EnemyPid, update_get)
    end, [Child || {_, Child, _, _} <- Children]).
