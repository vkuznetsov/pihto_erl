-module(images_store).

-export([get/2, search/2, save/3]).

-include("records.hrl").

-spec get(id(), id()) -> image() | notfound.
get(UserId, ImageId) ->
    poolboy:transaction(images_store_pool, fun(Worker) ->
        gen_server:call(Worker, {get, UserId, ImageId})
    end).

-spec search(id(), image_tag()) -> [id()].
search(UserId, Tag) ->
    poolboy:transaction(images_store_pool, fun(Worker) ->
        gen_server:call(Worker, {search, UserId, Tag})
    end).

-spec save(id(), id(), image()) -> ok.
save(UserId, ImageId, Image) ->
    poolboy:transaction(images_store_pool, fun(Worker) ->
        gen_server:call(Worker, {search, UserId, ImageId, Image})
    end).
