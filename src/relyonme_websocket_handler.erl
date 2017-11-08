-module(relyonme_websocket_handler).

-export([init/2, websocket_init/1, websocket_handle/3, websocket_info/2]).

-record(state, {
    game_room = undefined
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

websocket_handle({text, Data}, _Req, #state{game_room = GameRoomPid} = State) ->
    ClientCommand = jiffy:decode(Data, [return_maps]),
    case maps:get(<<"type">>, ClientCommand) of
        <<"key_command">> ->
            KeyCommand = maps:get(<<"key_command">>, ClientCommand),
            #{
                <<"key">> := Key,
                <<"command">> := Command
            } = KeyCommand,
            relyonme_game_room:client_key_command(GameRoomPid, self(), {Command, Key});
        _ ->
            ok
    end,
    {ok, State};

websocket_handle(Message, Req, State) ->

    {reply, Message, Req, State}.

websocket_info({update_active_pos, {X, Y}}, State) ->
    {reply, {text, construct_message(position_update, {X, Y})}, State};

websocket_info(_Info, State) ->
    {ok, State}.

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
    }).