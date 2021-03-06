-module(pihto_utils).

-export([get_all_image_ids/1, regenerate_thumbs/2, resave_images/1]).

%% -spec get_all_image_ids(pid()) -> [binary()].
get_all_image_ids(Riak) ->
  case riakc_pb_socket:list_keys(Riak, {<<"images">>, <<"images">>}) of
    {ok, Ids} -> Ids;
    {error, notfound} -> []
  end.

regenerate_thumbs(Type, ImageIds) ->
  Fun = fun(ImageId) ->
    Image = images:get(ImageId),
    {_, URL} = lists:keyfind(<<"url">>, 1, Image),

    case {ImageId, URL} of
      {undefined, _} -> io:format("Undefined id~n");
      {_, undefined} -> io:format("Undefined url ~s~n", [ImageId]);
      {_, _} ->
        io:format("Saving Id:~s URL:~s~n", [ImageId, URL]),
        pihto_thumbs:save_sync(Type, ImageId, URL)
    end
  end,

  lists:foreach(Fun, ImageIds).

resave_images(ImageIds) ->
  lists:foreach(fun resave_image/1, ImageIds).

resave_image(ImageId) ->
  Image = images:get(ImageId),

  case images:get(ImageId) of
    notfound ->
      io:format("Not found ~p~n", [ImageId]);
    Image ->
      Tags = case lists:keyfind(<<"tags">>, 1, Image) of
               {_, T} -> T;
               false -> [<<"notag">>]
             end,

      Props = lists:foldl(
        fun(Key, Acc) ->
          case lists:keyfind(Key, 1, Image) of
            false -> Acc;
            {_, undefined} -> Acc;
            {_, Value} -> [{Key, Value} | Acc]
          end
        end,
        [{<<"uid">>, ImageId}],
        allowed_keys()
      ),

      io:format("Saving ~s~n", [ImageId]),
      images:save(ImageId, Props, Tags)
  end.

allowed_keys() ->
  [
    <<"url">>,
    <<"origin">>,
    <<"referrer">>,
    <<"title">>,
    <<"comment">>,
    <<"added_at">>,
    <<"width">>,
    <<"height">>
  ].
