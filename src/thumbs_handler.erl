-module(thumbs_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Type, Req1} = case cowboy_req:binding(type, Req) of
                   {<<"thumb">>, R} -> {thumb, R};
                   {<<"image">>, R} -> {image, R}
                 end,

  case cowboy_req:binding(image_id, Req1) of
    {undefined, Req2} ->
      {ok, Req3} = cowboy_req:reply(400, [], <<"Bad Request">>, Req2),
      {ok, Req3, State};
    {ImageId, Req2} ->
      Thumb = thumbs:get(Type, ImageId),
      {ok, Reply} = cowboy_req:reply(200, [{<<"content-type">>, <<"image/jpg">>}], Thumb, Req2),
      {ok, Reply, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
