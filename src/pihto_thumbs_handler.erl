-module(pihto_thumbs_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    case cowboy_req:binding(image_id, Req) of
        {undefined, Req1} ->
            {ok, Req2} = cowboy_req:reply(400, [], <<"Bad Request">>, Req1),
            {ok, Req2, State};
        {ImageId, Req1} ->
            Thumb = pihto_thumbs:get(ImageId),
            {ok, Reply} = cowboy_req:reply(200, [{<<"content-type">>, <<"image/jpg">>}], Thumb, Req1),
            {ok, Reply, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
