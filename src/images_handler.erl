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
        [{{<<"application">>, <<"json">>, '*'}, save_image}],
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
    UserId = <<"vlas">>,
    case cowboy_req:binding(image_id, Req) of
        {undefined, Req1} ->
            {Tag, Req2} = cowboy_req:qs_val(<<"tag">>, Req1),
            ImageIds = images_store:search(UserId, Tag),
            JSON = jiffy:encode(ImageIds),
            {JSON, Req2, State};
        {ImageId, Req1} ->
            ImageJSON = images_store:get(UserId, ImageId),
            {ImageJSON, Req1, State}
    end.

save_image(Req, State) ->
    UserId = <<"vlas">>,
    {ok, ImageJSON, Req1} = cowboy_req:body(Req),
    {ImageId, Req2} = cowboy_req:binding(image_id, Req1),

    NotNullImageId = images_store:save(UserId, ImageId, ImageJSON),
    io:format("Saved ~p: ~s~n", [NotNullImageId, ImageJSON]),
    {true, Req2, State}.
