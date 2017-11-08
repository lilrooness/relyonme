-module(relyonme_http_handler).

-export([init/2]).

init(Req, State) ->
    Req0 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, <<"<h1>Hello World</h1>">>, Req),
    {ok, Req0, State}.
