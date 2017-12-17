-module(relyonme_websocket_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {
    game_room = undefined,
    map_data = undefined
    }).

init(Req, _State) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    % look for games to join before creating one . . .
    GamePids = [Pid || {_, Pid, _, _} <- supervisor:which_children(relyonme_games_sup)],

    FilterFun = fun(GamePid) ->
        relyonme_game_room:get_room_joinable(GamePid)
    end,

    GameRoomPid = case lists:filter(FilterFun, GamePids) of

        [JoinableGame | _] ->
            relyonme_game_room:join_room(JoinableGame, self()),
            JoinableGame;
        [] ->
            {ok, NewRoom} = supervisor:start_child(relyonme_games_sup, 
                relyonme_games_sup:childspec(_PlayerConnection = self())
            ),
            NewRoom
    end,

    Data = construct_message(room_number, relyonme_game_room:get_room_number(GameRoomPid)),

    {reply, {text, Data}, State#state{
        game_room = GameRoomPid
    }}.

websocket_handle({text, Data}, #state{game_room = GameRoomPid} = State) ->
    ClientCommand = jiffy:decode(Data, [return_maps]),
    case maps:get(<<"type">>, ClientCommand) of
        <<"key_command">> ->
            KeyCommand = maps:get(<<"key_command">>, ClientCommand),
            #{
                <<"key">> := Key,
                <<"command">> := Command
            } = KeyCommand,
            relyonme_game_room:client_key_command(GameRoomPid, self(), {Command, Key});
        <<"mouse_click">> ->
            MouseClick = maps:get(<<"mouse_click">>, ClientCommand),
            #{
                <<"x">> := Xpos,
                <<"y">> := Ypos
            } = MouseClick,
            relyonme_game_room:client_click_command(GameRoomPid, self(), #{x => Xpos, y => Ypos});
        _ ->
            ok
    end,
    {ok, State};

websocket_handle(Message, State) ->
    {reply, Message, State}.

websocket_info({position_update, {X, Y}}, State) ->
    {reply, {text, construct_message(position_update, {X, Y})}, State};

websocket_info({map_data, MapData}, State) ->
    {ok, State#state{
        map_data = MapData
    }};

websocket_info({enemy_position_update, EnemyPositions}, State) ->
	{reply, {text, construct_message(enemy_position_update, EnemyPositions)}, State};

websocket_info({vision_zone_update, VisionZones}, State) ->
    {reply, {text, construct_message(vision_zone_update, VisionZones)}, State};

websocket_info(_Info, State) ->
    {ok, State}.

construct_message(vision_zone_update, VisionZones) ->
    jiffy:encode(#{
        type => vision_zone_update,
        vision_zone_update => #{
            vision_zones => VisionZones
        }
    });

construct_message(room_number, RoomNumber) ->
    jiffy:encode(#{
        type => room_number,
        room_number => RoomNumber
    });

construct_message(position_update, {X, Y}) ->
    jiffy:encode(#{
        type => position_update,
        position_update => #{
            x => X,
            y => Y
        }
    });

construct_message(enemy_position_update, PositionData) ->
	MapList = [#{
		x => X,
		y => Y
	} || {X, Y} <- PositionData],
	jiffy:encode(#{
		type => enemy_position_update,
		enemy_position_update => MapList
	}).
