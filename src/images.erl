-module(images).
-behaviour(gen_server).

-export([start_link/0, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([search/1, get/1, save/3]).

search(Tag) ->
  gen_server:call(?MODULE, {search, Tag}).

get(ImageId) ->
  gen_server:call(?MODULE, {get, ImageId}).

save(ImageId, Data, Tags) ->
  gen_server:call(?MODULE, {save, ImageId, Data, Tags}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, _Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087).

terminate(_Reason, _State) ->
  ok.

handle_call({search, Tag}, _From, Riak) ->
  ImageIds = fetch_image_ids(Riak, Tag),
  Images = [{ImageId, fetch_image(Riak, ImageId)} || ImageId <- ImageIds],
  {reply, Images, Riak};

handle_call({get, ImageId}, _From, Riak) ->
  Image = fetch_image(Riak, ImageId),
  {reply, Image, Riak};

handle_call({save, ImageId, Data, Tags}, _From, Riak) ->
  update_image(Riak, ImageId, Data, Tags),
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
  case riakc_pb_socket:get(Riak, <<"images">>, ImageId) of
    {ok, Fetched} -> riakc_obj:get_value(Fetched);
    {error, notfound} -> notfound
  end.

update_image(Riak, ImageId, Data, Tags) ->
  Obj = riakc_obj:new(<<"images">>, ImageId, Data),
  ok = riakc_pb_socket:put(Riak, Obj),

  case riakc_pb_socket:fetch_type(Riak, {<<"tags">>, <<"image_tags">>}, ImageId) of
    {ok, ImageTagsSet} ->
      {set, CurrentTags, _, _, _} = ImageTagsSet,
      ExtraTags = lists:subtract(CurrentTags, Tags),
      NewTags = lists:subtract(Tags, CurrentTags);
    {error, {notfound, set}} ->
      ImageTagsSet = riakc_set:new(),
      ExtraTags = [],
      NewTags = Tags
  end,

  ImageTagsSet1 = del_image_tags_from_set(ExtraTags, ImageTagsSet),
  ImageTagsSet2 = add_image_tags_to_set(NewTags, ImageTagsSet1),
  riakc_pb_socket:update_type(Riak, {<<"tags">>, <<"image_tags">>}, ImageId, riakc_set:to_op(ImageTagsSet2)),

  ok = add_image_to_tags(ImageId, NewTags, Riak),
  ok = del_image_from_tags(ImageId, ExtraTags, Riak).


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

