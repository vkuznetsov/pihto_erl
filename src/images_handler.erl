-module(images_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 read_json(),
                 Req
                ),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

read_json() ->
  {ok, File} = file:read_file(filename:join(code:priv_dir(webserver), "images.json")),
  File.