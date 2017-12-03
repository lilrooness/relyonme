-module(relyonme_game_room).

-behaviour(gen_server).

-define(FIRST_JOIN_MODE, active).
-define(SECOND_JOIN_MODE, observe).

-define(INITIAL_ENEMY_COUNT, 5).

-define(WORLD_WIDTH, 200).
-define(WORLD_HEIGHT, 200).

-define(PLAYER_SPEED, 1).

-define(UPDATE_TIME, 17).

-define(SIGHT_RANGE, 30).

-export([
    start_link/2, 
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

% API
-export([
    get_room_number/1,
    get_room_joinable/1,
    join_room/2,
    client_click_command/3,
    client_key_command/3]).

-record(player, {
    mode = undefined, % observe | active
    connection = undefined,
    xpos = ?WORLD_WIDTH/2,
    ypos = ?WORLD_HEIGHT/2,
    client_keys_down = #{
        <<"w">> => false,
        <<"a">> => false,
        <<"s">> => false,
        <<"d">> => false,
        <<"-">> => false
    }
}).

-record(state, {
    room_number = undefined,
    player_1 = undefined,
    player_2 = undefined,
    ready = false,
    enemy_sup = undefined,
    vision_zones = []
}).

start_link(RoomNumber, PlayerConnection) ->
    gen_server:start_link(?MODULE, [RoomNumber, PlayerConnection], []).

init([RoomNumber, PlayerConnection]) ->
    {ok, EnemySupPid} = relyonme_enemy_sup:start_link(),

    lists:foreach(fun(_) -> 
        X = rand:uniform() * ?WORLD_WIDTH,
        Y = rand:uniform() * ?WORLD_HEIGHT,
        relyonme_enemy_sup:new_enemy(EnemySupPid, {X, Y})
    end, lists:seq(1, ?INITIAL_ENEMY_COUNT)),
    
    {ok, #state{
        player_1 = #player{
            connection = PlayerConnection,
            mode = ?FIRST_JOIN_MODE
        },
        room_number = RoomNumber,
        enemy_sup = EnemySupPid
    }}.

handle_call(get_room_number, _From, #state{room_number = RoomNumber} = State) ->
    {reply, RoomNumber, State};

handle_call(get_joinable, _From, #state{ready = Ready} = State) ->
    case Ready of
        true ->
            {reply, false, State};
        false ->
            {reply, true, State}
    end;

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({join_room, PlayerConnection}, #state{player_2 = undefined, ready = false} = State) ->
    Player2 = #player{
        connection = PlayerConnection,
        mode = ?SECOND_JOIN_MODE
    },
    self() ! ready,
    {noreply, State#state{player_2 = Player2}};

handle_cast({key_command, {PlayerConnection, {<<"key_down">>, Key}}}, #state{ready = true} = State) ->
    NewState = maybe_change_active_player_from_connection(fun(Player) ->
        #player{client_keys_down = ClientKeysDown} = Player,
        Player#player{client_keys_down = ClientKeysDown#{
            Key := true
        }}
    end, PlayerConnection, State),
    {noreply, NewState};

handle_cast({key_command, {PlayerConnection, {<<"key_up">>, Key}}}, #state{ready = true} = State) ->
    NewState = maybe_change_active_player_from_connection(fun(Player) ->
        #player{client_keys_down = ClientKeysDown} = Player,
        Player#player{client_keys_down = ClientKeysDown#{
            Key := false
        }}
    end, PlayerConnection, State),
    {noreply, NewState};

handle_cast({click_command, {PlayerConnection, ClickCommand}}, #state{ready = true} = State) ->
    PlayerObject = get_mode_player(State, observe),
    case PlayerConnection == PlayerObject#player.connection of
        true ->
            #{
                x := Xpos,
                y := Ypos
            } = ClickCommand,
            NewState = new_vision_zone(State, Xpos, Ypos),
            {noreply, NewState};
        false ->
            {noreply, State}
    end;

handle_cast(update_player_position, #state{ready = true} = State) ->
    NewState = change_active_player(fun(Player) ->
        ClientKeys = maps:keys(Player#player.client_keys_down),
        lists:foldl(fun(Key, AccPlayer) -> 
            case maps:get(Key, AccPlayer#player.client_keys_down) of
                true ->
                    _UpdatedPlayer = process_keydown_for_player(Key, AccPlayer);
                false ->
                    AccPlayer
            end
        end, Player, ClientKeys)
    end, State),
    {noreply, NewState};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(ready, State) ->
    self() ! update,
    {noreply, State#state{ready = true}};

handle_info(update, State) ->
    gen_server:cast(self(), update_player_position),
    ActivePlayer = get_mode_player(State, active),
    ObserverPlayer = get_mode_player(State, observe),
    Player1 = State#state.player_1,
    Player1#player.connection ! {position_update, {ActivePlayer#player.xpos, ActivePlayer#player.ypos}},

    Player2 = State#state.player_2,
    Player2#player.connection ! {position_update, {ActivePlayer#player.xpos, ActivePlayer#player.ypos}},
    
    EnemyPositions = relyonme_enemy_sup:update_get_enemy_positions(State#state.enemy_sup),
    ObserverPlayer#player.connection ! {
        enemy_position_update, 
        EnemyPositions
    },
    maybe_send_enemy_positions(ActivePlayer, EnemyPositions, State#state.vision_zones),
    erlang:send_after(?UPDATE_TIME, self(), update),
    {noreply, State};

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

get_room_number(Pid) ->
    gen_server:call(Pid, get_room_number).

get_room_joinable(Pid) ->
    gen_server:call(Pid, get_joinable).

join_room(Pid, PlayerConnection) ->
    gen_server:cast(Pid, {join_room, PlayerConnection}).

client_key_command(Pid, PlayerConnection, KeyCommand) ->
    gen_server:cast(Pid, {key_command, {PlayerConnection, KeyCommand}}).

client_click_command(Pid, PlayerConnection, ClickCommand) ->
    gen_server:cast(Pid, {click_command, {PlayerConnection, ClickCommand}}).

new_vision_zone(State, X, Y) ->
    NewState = State#state{
        vision_zones = [#{x => X, y => Y}] ++ State#state.vision_zones
    },
    Player2 = State#state.player_2,
    Player1 = State#state.player_1,
    Message = {
        vision_zone_update,
        NewState#state.vision_zones
    },
    Player2#player.connection ! Message,
    Player1#player.connection ! Message,
    NewState.

get_mode_player(State, Mode) ->
    if
        (State#state.player_1)#player.mode == Mode ->
            State#state.player_1;
        (State#state.player_2)#player.mode == Mode ->
            State#state.player_2
    end.

maybe_change_active_player_from_connection(Fun, PlayerConnection, State) ->
    if
        ((State#state.player_1)#player.mode == active) and ((State#state.player_1)#player.connection == PlayerConnection) ->
            UpdatedPlayer1 = Fun(State#state.player_1),
            State#state{player_1 = UpdatedPlayer1};
        ((State#state.player_2)#player.mode == active) and ((State#state.player_2)#player.connection == PlayerConnection) ->
            UpdatedPlayer2 = Fun(State#state.player_2),
            State#state{player_2 = UpdatedPlayer2};
        true ->
            State
    end.

change_active_player(Fun, State) ->
    if
        ((State#state.player_1)#player.mode == active) ->
            UpdatedPlayer1 = Fun(State#state.player_1),
            State#state{player_1 = UpdatedPlayer1};
        ((State#state.player_2)#player.mode == active) ->
            UpdatedPlayer2 = Fun(State#state.player_2),
            State#state{player_2 = UpdatedPlayer2};
        true ->
            State
    end.

process_keydown_for_player(Key, Player) ->
    case Key of
        <<"w">> ->
            Y = Player#player.ypos,
            Player#player{ypos = Y - ?PLAYER_SPEED};
        <<"a">> ->
            X = Player#player.xpos,
            Player#player{xpos = X - ?PLAYER_SPEED};
        <<"s">> ->
            Y = Player#player.ypos,
            Player#player{ypos = Y + ?PLAYER_SPEED};
        <<"d">> ->
            X = Player#player.xpos,
            Player#player{xpos = X + ?PLAYER_SPEED};
        _ ->
            Player
    end.

maybe_send_enemy_positions(Player, EnemyPositions, VisionZones) ->
    Positions = lists:filter(fun(E) ->
        case within_range(E, {Player#player.xpos, Player#player.ypos}, ?SIGHT_RANGE) of
            true ->
                true;
            _ ->
                case lists:any(fun(Vz) ->
                        #{x := Vx, y := Vy} = Vz,
                        within_range(E, {Vx, Vy}, ?SIGHT_RANGE)
                    end, VisionZones) of
                    true ->
                        true;
                    _ ->
                        false
                end
        end
    end, EnemyPositions),
    Player#player.connection ! {
        enemy_position_update,
        Positions
    }.

within_range({Ax, Ay} = _A, {Bx, By} = _B, Range) ->
    Dist = math:pow(Ax-Bx, 2) + math:pow(Ay-By, 2),
    Dist < math:pow(Range, 2).
