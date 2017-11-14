-module(relyonme_enemy).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-export([update_get/1]).

-record(state, {
    position = {0, 0},
    velocity = {0, 0},
    ttl = 5000
    }).

start_link(StartingPos) ->
    gen_server:start_link(?MODULE, [StartingPos], []).

init([StartingPos]) ->
    {ok, #state{
        position = StartingPos
    }}.

handle_call(update_get, _From, #state{ttl = 0} = State) ->
    self() ! die,
    {reply, State#state.position, State};

handle_call(update_get, _From, State) ->
    #state{
        position = {
            XPos, YPos
        },
        velocity = {
            XVel, YVel
        }
    } = State,
    UpdatedPosition = {
        XPos + XVel,
        YPos + YVel
    },
    UpdatedState = State#state{
        position = UpdatedPosition,
        ttl = State#state.ttl - 1
    },
    {reply, UpdatedState#state.position, UpdatedState};

handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast({set_velocity, X, Y}, State) ->
    {noreply, State#state{velocity = {X, Y}}};

handle_cast({set_position, X, Y}, State) ->
    {noreply, State#state{position = {X, Y}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(die, State) ->
    gen_server:stop(self()),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

update_get(Pid) ->
    gen_server:call(Pid, update_get).
