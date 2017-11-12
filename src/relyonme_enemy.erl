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
    position = undefined
    }).

start_link(StartingPos) ->
    gen_server:start_link(?MODULE, [StartingPos], []).

init([StartingPos]) ->
    {ok, #state{
        position = StartingPos
    }}.

handle_call(update_get, _From, State) ->
    {reply, State#state.position, State};

handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

update_get(Pid) ->
    gen_server:call(Pid, update_get).
