%%%-------------------------------------------------------------------
%% @doc relyonme public API
%% @end
%%%-------------------------------------------------------------------

-module(relyonme_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/relyonme/[...]", cowboy_static, {priv_dir, relyonme, "client"}},
			{"/ws", relyonme_websocket_handler, []},
			{"/list_games", relyonme_http_handler, []}
		]}
		% {'_', []}
	]),
	cowboy:start_clear(my_http_listener,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
    {ok, WorldData} = file:read_file("priv/maps/worlds.json"),
    WorldsMap = jiffy:decode(WorldData, [return_maps]),
    application:set_env(relyonme, world_data, WorldsMap),
    relyonme_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
