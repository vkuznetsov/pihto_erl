-module(images_handler).

-export([
  init/3,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  delete_resource/2
]).

-export([
  get_image/2,
  save_image/2
]).

init(_Transport, _Req, _Opts) ->
  random:seed(now()),
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
  {
   [{{<<"application">>, <<"json">>, []}, get_image}],
   Req, State
  }.

content_types_accepted(Req, State) ->
  {
   [{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, save_image}],
   Req, State
  }.

delete_resource(Req, State) ->
  case cowboy_req:binding(image_id, Req) of
    {undefined, Req1} ->
      {ok, Req2} = cowboy_req:reply(400, Req1),
      {halt, Req2, State};
    {ImageId, Req1} ->
      images:delete(ImageId),
      {true, Req1, State}
  end.

get_image(Req, State) ->
  case cowboy_req:binding(image_id, Req) of
    {undefined, Req1} ->
      {Tag, Req2} = cowboy_req:qs_val(<<"tag">>, Req1),
      Images = images:search(Tag),
      %% JSON = images_to_json(Images),
      JSON = jiffy:encode(Images),
      {JSON, Req2, State};
    {ImageId, Req1} ->
      Image = images:get(ImageId),
      JSON = jiffy:encode({Image}),
      {JSON, Req1, State}
  end.

save_image(Req, State) ->
  {ok, Data, Req1} = cowboy_req:body_qs(Req),

  URL = proplists:get_value(<<"url">>, Data),
  ImageId = list_to_binary(md5:md5_hex(URL)),
  Tags = case get_tags(Data) of
           [] -> [<<"notag">>];
           T -> T
         end,

  AllowedKeys = [
                 <<"url">>,
                 <<"origin">>,
                 <<"referrer">>,
                 <<"title">>,
                 <<"comment">>,
                 <<"added_at">>,
                 <<"width">>,
                 <<"height">>
                ],

  FilteredParams = lists:foldl(
                     fun(Key, NewList) ->
                         case proplists:get_value(Key, Data) of
                           undefined -> NewList;
                           Value -> [{Key, Value} | NewList]
                         end
                     end,
                     [{<<"uid">>, ImageId}],
                     AllowedKeys
                    ),

  images:save(ImageId, FilteredParams, Tags),
  thumbs:save_async(image, ImageId, URL),
  thumbs:save_async(thumb, ImageId, URL),
  io:format("Saved: ~p Tags: ~p~n", [FilteredParams, Tags]),
  {true, Req1, State}.

%% Дальше следует уёбство. Не смотреть!

get_tags(Data) -> get_tags_with_index(Data, 0, []).

get_tags_with_index(Data, Num, Tags) ->
  ParamName = list_to_binary(io_lib:format("tags[~b]", [Num])),
  case proplists:get_value(ParamName, Data) of
    undefined -> get_tags_without_index(Data, Tags);
    Tag -> get_tags_with_index(Data, Num + 1, [Tag | Tags])
  end.

get_tags_without_index([], Tags) ->
  lists:reverse(Tags);
get_tags_without_index([{ParamName, ParamValue} | Data], Tags) ->
  case ParamName of
    <<"tags[]">> -> get_tags_without_index(Data, [ParamValue | Tags]);
    _ -> get_tags_without_index(Data, Tags)
  end.

%% images_to_json(Images) ->
%%   {BinaryString, _} = lists:foldl(
%%                         fun(Image, {Binary, Empty}) ->
%%                             Encoded = jiffy:encode({Image}),
%%                             if
%%                               Empty -> {<<"[", Encoded/binary>>, false};
%%                               true -> {<<Binary/binary, ",", Encoded/binary>>, false}
%%                             end
%%                         end,
%%                         {<<>>, true},
%%                         Images
%%                        ),
%%   <<BinaryString/binary, "]">>.

%% image_to_json(Image) ->
%%   jiffy:encode({Image}).
