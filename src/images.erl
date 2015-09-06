-module(images).
-behaviour(gen_server).

-include_lib("riakc/include/riakc.hrl").

-export([start_link/1, init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([search/1, get/1, save/3, delete/1]).

search(Tag) ->
    poolboy:transaction(images_pool, fun(Worker) ->
        gen_server:call(Worker, {search, Tag})
    end).

get(ImageId) ->
    poolboy:transaction(images_pool, fun(Worker) ->
        gen_server:call(Worker, {get, ImageId})
    end).

save(ImageId, Data, Tags) ->
    poolboy:transaction(images_pool, fun(Worker) ->
        gen_server:call(Worker, {save, ImageId, Data, Tags})
    end).

delete(ImageId) ->
  gen_server:call(?MODULE, {delete, ImageId}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([]) ->
  {ok, _Riak} = riakc_pb_socket:start_link("127.0.0.1", 8087).

terminate(_Reason, _State) ->
  ok.

handle_call({search, Tag}, _From, Riak) ->
  Images = search(Riak, Tag),
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

search(Riak, Tag) ->
  Request = <<"tags_set:", Tag/binary>>,
  {ok, Results} = riakc_pb_socket:search(Riak, <<"images_index">>, Request, [{rows, 100}, {sort, <<"added_at_register desc">>}]),
  Docs = Results#search_results.docs,

  ImageIds = lists:foldr(
               fun({_Index, Doc}, Acc) ->
                   {_, ImageId} = lists:keyfind(<<"_yz_rk">>, 1, Doc),
                   [ImageId | Acc]
               end, [], Docs
              ),
  ImageIds.

fetch_image(Riak, ImageId) ->
  case riakc_pb_socket:fetch_type(Riak, {<<"images">>, <<"images">>}, ImageId) of
    {ok, {map, Image, _, _, _}} -> [{Key, Value} || {{Key, _}, Value} <- Image];
    {error, {notfound, map}} -> notfound
  end.

update_image(Riak, ImageId, Props, Tags) ->
  riakc_pb_socket:modify_type(Riak,
                              fun(Map) ->
                                  Map1 = update_image_tags(Map, Tags),
                                  update_image_fields(Map1, Props)
                              end,
                              {<<"images">>, <<"images">>},
                              ImageId,
                              [create]
                             ).

update_image_tags(ImageMap, Tags) ->
  riakc_map:update(
    {<<"tags">>, set},
    fun(TagsSet) ->
        CurrentTags = riakc_set:value(TagsSet),
        %% NewTags = lists:subtract(Tags, CurrentTags),
        NewTags = Tags,
        ExtraTags = lists:subtract(CurrentTags, Tags),

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

add_image_tags_to_set([], RiakSet) -> RiakSet;
add_image_tags_to_set([Tag | Tags], RiakSet) ->
  RiakSet1 = riakc_set:add_element(Tag, RiakSet),
  add_image_tags_to_set(Tags, RiakSet1).

del_image_tags_from_set([], RiakSet) -> RiakSet;
del_image_tags_from_set([Tag | Tags], RiakSet) ->
  RiakSet1 = riakc_set:del_element(Tag, RiakSet),
  del_image_tags_from_set(Tags, RiakSet1).

delete_image(Riak, ImageId) ->
  riakc_pb_socket:delete(Riak, {<<"images">>, <<"images">>}, ImageId).
