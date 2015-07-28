-module(images).
-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([search/1, get/1, save/3, delete/1]).

search(Tag) ->
  gen_server:call(?MODULE, {search, Tag}).

get(ImageId) ->
  gen_server:call(?MODULE, {get, ImageId}).

save(ImageId, Data, Tags) ->
  gen_server:call(?MODULE, {save, ImageId, Data, Tags}).

delete(ImageId) ->
  gen_server:call(?MODULE, {delete, ImageId}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, _Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087).

terminate(_Reason, _State) ->
  ok.

handle_call({search, Tag}, _From, Riak) ->
  ImageIds = fetch_image_ids(Riak, Tag),
  Images = [fetch_image(Riak, ImageId) || ImageId <- ImageIds],
  {reply, Images, Riak};

handle_call({get, ImageId}, _From, Riak) ->
  Image = fetch_image(Riak, ImageId),
  {reply, Image, Riak};

handle_call({save, ImageId, Data, Tags}, _From, Riak) ->
  update_image(Riak, ImageId, Data, Tags),
  {reply, ok, Riak};

handle_call({delete, ImageId}, _From, Riak) ->
  delete_image(Riak, ImageId),
  {reply, ok, Riak}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(_Message, State) ->
  {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

fetch_image_ids(Riak, Tag) ->
  case riakc_pb_socket:fetch_type(Riak, {<<"tags">>, <<"tag_images">>}, Tag) of
    {ok, {set, ImageIds, _, _, _}} -> ImageIds;
    {error, {notfound, set}} -> []
  end.

fetch_image(Riak, ImageId) ->
  case riakc_pb_socket:fetch_type(Riak, {<<"images">>, <<"images">>}, ImageId) of
    {ok, {map, Image, _, _, _}} -> [{Key, Value} || {{Key, _}, Value} <- Image];
    {error, {notfound, map}} -> notfound
  end.

update_image(Riak, ImageId, Props, Tags) ->
  riakc_pb_socket:modify_type(Riak,
    fun(Map) ->
      Map1 = update_image_tags(ImageId, Map, Tags, Riak),
      update_image_fields(Map1, Props)
    end,
    {<<"images">>, <<"images">>},
    ImageId,
    [create]
  ).

update_image_tags(ImageId, ImageMap, Tags, Riak) ->
  riakc_map:update(
    {<<"tags">>, set},
    fun(TagsSet) ->
      CurrentTags = riakc_set:value(TagsSet),
      NewTags = lists:subtract(Tags, CurrentTags),
      ExtraTags = lists:subtract(CurrentTags, Tags),

      ok = add_image_to_tags(ImageId, NewTags, Riak),
      ok = del_image_from_tags(ImageId, ExtraTags, Riak),

      TagsSet1 = add_image_tags_to_set(NewTags, TagsSet),
      del_image_tags_from_set(ExtraTags, TagsSet1)

    end,
    ImageMap
  ).

update_image_fields(ImageMap, Props) ->
  lists:foldl(
    fun({Key, Value}, Map) ->
      riakc_map:update(
        {Key, register},
        fun(Reg) -> riakc_register:set(Value, Reg) end,
        Map
      )
    end,
    ImageMap,
    Props
  ).

add_image_to_tags(_ImageId, [], _Riak) -> ok;
add_image_to_tags(ImageId, [Tag | Tags], Riak) ->
  riakc_pb_socket:modify_type(Riak,
    fun(Set) -> riakc_set:add_element(ImageId, Set)
    end, {<<"tags">>, <<"tag_images">>}, Tag, []
  ),
  add_image_to_tags(ImageId, Tags, Riak).

del_image_from_tags(_ImageId, [], _Riak) -> ok;
del_image_from_tags(ImageId, [Tag | Tags], Riak) ->
  riakc_pb_socket:modify_type(Riak,
    fun(Set) -> riakc_set:del_element(ImageId, Set)
    end, {<<"tags">>, <<"tag_images">>}, Tag, []
  ),
  del_image_from_tags(ImageId, Tags, Riak).

add_image_tags_to_set([], RiakSet) -> RiakSet;
add_image_tags_to_set([Tag | Tags], RiakSet) ->
  RiakSet1 = riakc_set:add_element(Tag, RiakSet),
  add_image_tags_to_set(Tags, RiakSet1).

del_image_tags_from_set([], RiakSet) -> RiakSet;
del_image_tags_from_set([Tag | Tags], RiakSet) ->
  RiakSet1 = riakc_set:del_element(Tag, RiakSet),
  del_image_tags_from_set(Tags, RiakSet1).

delete_image(Riak, ImageId) ->
  case fetch_image(Riak, ImageId) of
    notfound -> notfound;
    Image ->
      Tags = proplists:get_value(<<"tags">>, Image),
      del_image_from_tags(ImageId, Tags, Riak),
      riakc_pb_socket:delete(Riak, {<<"images">>, <<"images">>}, ImageId)
  end.