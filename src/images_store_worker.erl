-module(images_store_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("riakc/include/riakc.hrl").
-include("records.hrl").

-record(state, {riak}).

-define(IMAGES_INDEX(UserId), <<UserId/binary, "_images_index">>).
-define(IMAGES_BUCKET(UserId), <<UserId/binary, "_images">>).
-define(THUMBS_BUCKET(UserId), <<UserId/binary, "_thumbs">>).
-define(TAGS_BUCKET(UserId), {<<"tags">>, <<UserId/binary, "_tags">>}).

-define(ALLOWED_IMAGES_PROPS, [<<"url">>,
                               <<"origin">>,
                               <<"referrer">>,
                               <<"title">>,
                               <<"comment">>,
                               <<"added_at">>,
                               <<"width">>,
                               <<"height">>]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    {_, RiakHost} = lists:keyfind(riak_host, 1, Args),
    {_, RiakPort} = lists:keyfind(riak_port, 1, Args),

    {ok, Riak} = riakc_pb_socket:start_link(RiakHost, RiakPort),
    {ok, #state{riak=Riak}}.

%% GenServer Callbacks

handle_call({get, UserId, ImageId}, _From, #state{riak=Riak}=State) ->
    Image = get(Riak, UserId, ImageId),
    {reply, Image, State};

handle_call({save, UserId, ImageId, Image}, _From, #state{riak=Riak}=State) ->
    ok = save(Riak, UserId, ImageId, Image),
    {reply, ok, State};

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

%% Private

-spec get(pid(), id(), id()) -> image() | notfound.
get(Riak, UserId, ImageId) ->
    case riakc_pb_socket:get(Riak, ?IMAGES_BUCKET(UserId), ImageId) of
        {error, notfound} -> notfound;
        {ok, Obj} -> riakc_obj:get_value(Obj)
    end.

-spec save(pid(), id(), id(), image()) -> ok.
save(Riak, UserId, ImageId, Image) ->
    Props = jiffy:decode(Image),

    {_, URL} = lists:keyfind(<<"url">>, 1, Props),

    {ok, Tags} = case lists:keyfind(<<"tags">>, 1, Props) of
                     false -> [<<"notag">>];
                     {_, T} -> T
                 end,

    save_image(Riak, UserId, ImageId, Props),
    save_thumb(Riak, UserId, ImageId, URL),
    add_tags(Riak, UserId, Tags),

    ok.

-spec search(pid(), id(), tag()) -> [id()].
search(Riak, UserId, Tag) ->
    Request = <<"tags_set:", Tag/binary>>,
    {ok, Results} = riakc_pb_socket:search(Riak,
                                           ?IMAGES_INDEX(UserId),
                                           Request,
                                           [{rows, 100}, {sort, <<"added_at_register desc">>}]),
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

    Obj = riakc_obj:new(?IMAGES_BUCKET(UserId), jiffy:encode({FilteredProps}), ImageId),
    {ok, _} = riakc_pb_socket:put(Riak, Obj).

-spec save_thumb(pid(), id(), id(), binary()) -> any().
save_thumb(Riak, UserId, ImageId, URL) ->
    Thumb = thumbs_pool:generate_thumb(URL),
    Obj = riakc_obj:new(?THUMBS_BUCKET(UserId), Thumb, ImageId),
    {ok, _} = riakc_pb_socket:put(Riak, Obj).

-spec add_tags(pid(), id(), [image_tag()]) -> any().
add_tags(Riak, UserId, Tags) ->
    riakc_pb_socket:modify_type(Riak, fun(Set) ->
        lists:foldl(fun(Tag, FSet) ->
            riakc_set:add_element(Tag, FSet)
                    end, Set, Tags)
    end, ?TAGS_BUCKET(UserId), UserId, [create]).
