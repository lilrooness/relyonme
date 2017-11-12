-module(relyonme_http_handler).

-export([init/2]).

init(Req, State) ->
	Games = [#{
		<<"game_number">> => integer_to_binary(GameNumber),
		<<"joinable">> => Joinable
	} || {GameNumber, Joinable} <- relyonme_games_sup:list_current_games()],
    
	GamesData = jiffy:encode(#{<<"games">> => Games}),

    Req0 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, GamesData, Req),
    {ok, Req0, State}.
