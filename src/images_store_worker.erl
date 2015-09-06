-module(images_store_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("riakc/include/riakc.hrl").
-include("records.hrl").

-record(state, {riak :: pid()}).

-define(IMAGES_INDEX, <<"images_index3">>).
-define(IMAGES_BUCKET(UserId), {<<"images_type3">>, <<UserId/binary, "_images">>}).
-define(TAGS_BUCKET, {<<"tags_type3">>, <<"tags">>}).

-define(ALLOWED_IMAGES_PROPS, [<<"url">>,
                               <<"origin">>,
                               <<"referrer">>,
                               <<"title">>,
                               <<"comment">>,
                               <<"tags">>,
                               <<"added_at">>,
                               <<"width">>,
                               <<"height">>]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    process_flag(trap_exit, true),
    {_, RiakHost} = lists:keyfind(riak_host, 1, Args),
    {_, RiakPort} = lists:keyfind(riak_port, 1, Args),

    {ok, Riak} = riakc_pb_socket:start_link(RiakHost, RiakPort),
    {ok, #state{riak=Riak}}.

handle_call({get, UserId, ImageId}, _From, #state{riak=Riak}=State) ->
    Image = get(Riak, UserId, ImageId),
    {reply, Image, State};

handle_call({delete, UserId, ImageId}, _From, #state{riak=Riak}=State) ->
    Result = delete(Riak, UserId, ImageId),
    {reply, Result, State};

handle_call({save, UserId, ImageId, Image}, _From, #state{riak=Riak}=State) ->
    NotNullImageId = save(Riak, UserId, ImageId, Image),
    {reply, NotNullImageId, State};

handle_call({search, UserId, Tag}, _From, #state{riak=Riak}=State) ->
    Images = search(Riak, UserId, Tag),
    {reply, Images, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get(pid(), id(), id()) -> image() | notfound.
get(Riak, UserId, ImageId) ->
    case riakc_pb_socket:get(Riak, ?IMAGES_BUCKET(UserId), ImageId) of
        {error, notfound} -> notfound;
        {ok, Obj} -> riakc_obj:get_value(Obj)
    end.

-spec delete(pid(), id(), id()) -> ok | notfound.
delete(Riak, UserId, ImageId) ->
    case riakc_pb_socket:delete(Riak, ?IMAGES_BUCKET(UserId), ImageId) of
        {error, notfound} -> notfound;
        ok -> ok
    end.

-spec save(pid(), id(), id(), image()) -> id().
save(Riak, UserId, ImageId, Image) ->
    {Props} = jiffy:decode(Image),

    NotNullImageId = case ImageId of
                         undefined -> image_id(Props);
                         V -> V
                     end,

    {_, URL} = lists:keyfind(<<"url">>, 1, Props),

    Tags = case lists:keyfind(<<"tags">>, 1, Props) of
               false -> [<<"notag">>];
               {_, T} -> T
           end,

    save_thumb(NotNullImageId, URL),
    save_image(Riak, UserId, NotNullImageId, Props),
    add_tags(Riak, UserId, Tags),
    NotNullImageId.

-spec search(pid(), id(), tag()) -> [id()].
search(Riak, UserId, Tag) ->
    Request = <<"tags:", Tag/binary, " AND _yz_rb:", UserId/binary, "_images">>,
    {ok, Results} = riakc_pb_socket:search(Riak,
                                           ?IMAGES_INDEX,
                                           Request,
                                           [{rows, 100}, {sort, <<"added_at desc">>}]),
    Docs = Results#search_results.docs,

    ImageIds = lists:foldr(
        fun({_Index, Doc}, Acc) ->
            {_, ImageId} = lists:keyfind(<<"_yz_rk">>, 1, Doc),
            [ImageId | Acc]
        end, [], Docs
    ),
    ImageIds.

-spec save_image(pid(), id(), id(), image_props()) -> any().
save_image(Riak, UserId, ImageId, Props) ->
    FilteredProps = lists:foldl(
        fun(Key, NewList) ->
            case lists:keyfind(Key, 1, Props) of
                false -> NewList;
                {_, Value} -> [{Key, Value} | NewList]
            end
        end,
        [{<<"uid">>, ImageId}],
        ?ALLOWED_IMAGES_PROPS
    ),

    Obj = riakc_obj:new(?IMAGES_BUCKET(UserId), ImageId, jiffy:encode({FilteredProps}), <<"application/json">>),
    ok = riakc_pb_socket:put(Riak, Obj).

-spec save_thumb(id(), binary()) -> any().
save_thumb(ImageId, URL) ->
    poolboy:transaction(thumb_pool, fun(Worker) ->
        gen_server:cast(Worker, {save, ImageId, URL})
    end).

-spec add_tags(pid(), id(), [image_tag()]) -> any().
add_tags(Riak, UserId, Tags) ->
    riakc_pb_socket:modify_type(Riak, fun(Set) ->
        lists:foldl(fun(Tag, FSet) ->
                        riakc_set:add_element(Tag, FSet)
                    end, Set, Tags)
    end, ?TAGS_BUCKET, UserId, [create]).

-spec image_id(image_props()) -> binary().
image_id(ImageProps) ->
    {_, URL} = lists:keyfind(<<"url">>, 1, ImageProps),
    SHA = erlsha2:sha256(URL),
    hmac:hexlify(SHA, [lower, binary]).
