-module(images_handler).

-export([
  init/3,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2
]).

-export([
  get_image/2,
  save_image/2
]).

init(_Transport, _Req, _Opts) ->
  random:seed(now()),
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {
    [{{<<"application">>, <<"json">>, []}, get_image}],
    Req, State
  }.

content_types_accepted(Req, State) ->
  {
    [{{<<"application">>, <<"x-www-form-urlencoded">>, []}, save_image}],
    Req, State
  }.

get_image(Req, State) ->
  case cowboy_req:binding(image_id, Req) of
    {undefined, Req1} ->
      images:search(""),
      {true, Req1, State};
    {ImageId, Req1} ->
      images:get(ImageId),
      {true, Req1, State};
    _ ->
      {false, Req, State}
  end.

save_image(Req, State) ->
  {ok, Data, Req1} = cowboy_req:body(Req),
  {ImageId, Req2} = cowboy_req:binding(image_id, Req1),
  images:save(ImageId, Data),
  {true, Req2, State}.
