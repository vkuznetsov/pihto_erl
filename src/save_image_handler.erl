-module(save_image_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3, image2json/4]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  URL = proplists:get_value(<<"url">>, Params),
  Origin = proplists:get_value(<<"origin">>, Params),
  Title = proplists:get_value(<<"title">>, Params),
  Comment = proplists:get_value(<<"comment">>, Params),

  save_image(URL, Origin, Title, Comment),
  io:format("Saved: ~s~n", [URL]),

  {ok, Req3} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 "{\"result\":\"OK\"}",
                 Req2
                ),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

save_image(URL, Origin, Title, Comment) ->
  FileName = filename:join(code:priv_dir(webserver), "images.json"),
  ok = file:write_file(FileName, image2json(URL, Origin, Title, Comment), [append]).

image2json(URL, Origin, Title, Comment) ->
  {{Year, Month, Day}, {Hour, Min, _Sec}} = erlang:localtime(),

  UID = md5:md5_hex(URL),
  AddedAt = io_lib:format("~4B-~2..0B-~2..0B ~2..0B:~2..0B", [Year, Month, Day, Hour, Min]),

  io_lib:format(
    "{\"uid\":\"~s\",\"title\":\"~s\",\"origin\":\"~s\",\"url\":\"~s\",\"added_at\":\"~s\",\"comment\":\"~s\"}",
    [UID, Title, Origin, URL, AddedAt, Comment]
  ).