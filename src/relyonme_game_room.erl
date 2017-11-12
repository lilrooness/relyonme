-module(relyonme_game_room).

-behaviour(gen_server).

-define(FIRST_JOIN_MODE, active).
-define(SECOND_JOIN_MODE, observe).

-define(WORLD_WIDTH, 200).
-define(WORLD_HEIGHT, 200).

-define(PLAYER_SPEED, 1).

-define(UPDATE_TIME, 17).

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
    enemy_sup = undefined
}).

start_link(RoomNumber, PlayerConnection) ->
    gen_server:start_link(?MODULE, [RoomNumber, PlayerConnection], []).

init([RoomNumber, PlayerConnection]) ->
    EnemySup = relyonme_enemy_sup:start_link(),
    {ok, #state{
        player_1 = #player{
            connection = PlayerConnection,
            mode = ?FIRST_JOIN_MODE
        },
        room_number = RoomNumber,
        enemy_sup = EnemySup
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

handle_call(Message, _From, State) ->
    {reply, Message, State}.

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

handle_cast(update_position, #state{ready = true} = State) ->
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


handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(ready, State) ->
    self() ! update,
    {noreply, State#state{ready = true}};

handle_info(update, State) ->
    gen_server:cast(self(), update_position),
    ActivePlayer = get_active_player(State),
    Player1 = State#state.player_1,
    Player1#player.connection ! {update_active_pos, {ActivePlayer#player.xpos, ActivePlayer#player.ypos}},

    Player2 = State#state.player_2,
    Player2#player.connection ! {update_active_pos, {ActivePlayer#player.xpos, ActivePlayer#player.ypos}},
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

get_active_player(State) ->
    if
        (State#state.player_1)#player.mode == active ->
            State#state.player_1;
        (State#state.player_2)#player.mode == active ->
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