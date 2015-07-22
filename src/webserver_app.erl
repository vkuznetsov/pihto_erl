-module(webserver_app).

-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/index.html", cowboy_static, {priv_file, webserver, "index.html"}},
                                           {"/js/[...]", cowboy_static, {priv_dir, webserver, "js"}},
                                           {"/css/[...]", cowboy_static, {priv_dir, webserver, "css"}},
                                           {"/fonts/[...]", cowboy_static, {priv_dir, webserver, "fonts"}},
                                           {"/:user_id/[:request]", images_handler, []},
                                           {"/", hello_handler, []}
                                          ]}
                                   ]),
  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]
                             ),
  webserver_sup:start_link().

stop(_State) ->
  ok.
