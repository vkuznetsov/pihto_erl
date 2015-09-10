-module(pihto_image_redirect_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ImageId, Req1} = cowboy_req:binding(image_id, Req),

  case pihto_images:get(<<"vlas">>, ImageId) of
    notfound -> {ok, notfound(Req1), State};
    Image ->
      {URL, Req2} = case cowboy_req:binding(target_type, Req1) of
                   {<<"src">>, R} -> {proplists:get_value(<<"url">>, Image), R};
                   {<<"origin">>, R} -> {proplists:get_value(<<"origin">>, Image), R}
                 end,
      {ok, redirect(URL, Req2), State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.

redirect(URL, Req) ->
  io:format("Redirect to ~s~n", [URL]),
  Body = <<"<head><meta http-equiv=\"refresh\" content=\"0; url=", URL/binary, "\"/></head>">>,
  {ok, Req1} = cowboy_req:reply(200, [], Body, Req),
  %% {ok, Req1} = cowboy_req:reply(301, [{<<"location">>, URL}], <<"Redirect to ", URL/binary>>, Req),
  Req1.

notfound(Req) ->
  {ok, Req1} = cowboy_req:reply(404, [], <<"Not Found">>, Req),
  Req1.
