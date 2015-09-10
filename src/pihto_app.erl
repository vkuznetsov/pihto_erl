-module(pihto_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([log_request/1, log_response/4]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  random:seed(now()),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, pihto, "index.html"}},
      {"/t", cowboy_static, {priv_file, pihto, "index2.html"}},
      {"/js/[...]", cowboy_static, {priv_dir, pihto, "js"}},
      {"/css/[...]", cowboy_static, {priv_dir, pihto, "css"}},
      {"/fonts/[...]", cowboy_static, {priv_dir, pihto, "fonts"}},
      {"/img/[...]", cowboy_static, {priv_dir, pihto, "img"}},
      {"/st/:type/:image_id", pihto_thumbs_handler, []},
      {"/image/:target_type/:image_id", pihto_image_redirect_handler, []},
      {"/images/[:image_id]", pihto_images_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [
      {env, [{dispatch, Dispatch}]},
      {onrequest, fun ?MODULE:log_request/1},
      {onresponse, fun ?MODULE:log_response/4}
    ]
  ),
  pihto_sup:start_link().

stop(_State) ->
  ok.

log_request(Req) ->
  {RequestId, Req1} = case cowboy_req:header(<<"x-request-id">>, Req) of
    {undefined, R} -> {pihto_md5:md5_hex(crypto:strong_rand_bytes(32)), R};
    {Val, R} -> {Val, R}
  end,

  Req2 = cowboy_req:set_resp_header(<<"x-request-id">>, RequestId, Req1),

  {Method, Req3} = cowboy_req:method(Req2),
  {URL, Req4} = cowboy_req:url(Req3),
  {ok, Body, _} = cowboy_req:body_qs(Req4),
  io:format("REQUEST [~s] ~s ~s ~p~n", [RequestId, Method, URL, Body]),
  Req4.

log_response(Status, Headers, _Body, Req) ->
  io:format("RESPONSE [~s] Status: ~p~n", [proplists:get_value(<<"x-request-id">>, Headers), Status]),
  Req.
